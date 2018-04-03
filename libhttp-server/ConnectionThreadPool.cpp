#include "ConnectionThreadPool.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Logger.hpp>
#include "MaxConnectionException.hpp"


namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::inst().getObservingLogger(__FILE__);

	/**
	 * @brief ConnectionTask
	 */

	ConnectionTask::ConnectionTask(AutoRef<Connection> connection, AutoRef<Communication> communication) {
		setConnection(connection, communication);
	}
	ConnectionTask::~ConnectionTask() {
	}

	void ConnectionTask::setConnection(AutoRef<Connection> connection, AutoRef<Communication> communication) {
		this->connection = connection;
		this->communication = communication;
	}

	void ConnectionTask::onTask() {
		try {
			connectionTask();
		} catch (NullException & e) {
			logger->error(e.toString());
		}
	}

	AutoRef<Connection> ConnectionTask::getConnection() {
		return connection;
	}

	void ConnectionTask::connectionTask() {
		connection->registerSelector(selector, Selector::READ | Selector::WRITE);
		try {
			connection->negotiate();
            communication->onConnected(connection);
            while (!isCanceled() && !connection->isTerminateFlaged()) {
				testReceiveTimeout();
                if (selector.select(1000) > 0) {
                    if (doReceive() == false && doWrite() == false) {
                        idle(10);
                    }
                }				
				if (closing()) {
					break;
				}
            }
        } catch (IOException & e) {
            logger->error(e.toString());
        }
        
        // notify disconnection to connection manager
        communication->onDisconnected(connection);

		connection->unregisterSelector(selector, Selector::READ | Selector::WRITE);
        connection->close();
        connection->completed() = true;
        
		communication = NULL;
	}

	void ConnectionTask::testReceiveTimeout() {
		if (connection->expiredRecvTimeout()) {
			throw IOException("recv timeout");
		}
	}

	bool ConnectionTask::doReceive() {
		bool proc = false;
		if (connection->isReadable(selector)) {
			do {
				if (communication->onReceivable(connection)) {
					proc = true;
				}
			} while (connection->socket()->pending() > 0 && communication->isReadable());
		}
		return proc;
	}

	bool ConnectionTask::doWrite() {
		bool proc = false;
		if (connection->isWritable(selector)) {
			if (communication->onWriteable(connection)) {
				proc = true;
			}
		}
		return proc;
	}

	bool ConnectionTask::closing() {
		return (connection->isClosed() || communication->isCommunicationCompleted());
	}

	/**
	 * @brief ConnectionThreadPool
	 */

	ConnectionThreadPool::ConnectionThreadPool(size_t maxThreads) : ThreadPool(maxThreads)  {
	}

	ConnectionThreadPool::~ConnectionThreadPool() {
	}

	void ConnectionThreadPool::createConnection(AutoRef<Communication> communication, AutoRef<Connection> connection) {
		StatefulThread * thread = acquire();
		if (thread == NULL) {
			throw MaxConnectionException("ConnectionThreadPool error - no thread available");
		}
		thread->task() = AutoRef<Task>(new ConnectionTask(connection, communication));
		enqueue(thread);
	}
}
