#include "ConnectionThreadPool.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

	/**
	 * @brief ConnectionThread
	 */

	ConnectionThread::ConnectionThread() {
	}
	ConnectionThread::~ConnectionThread() {
	}

	void ConnectionThread::setConnection(AutoRef<Connection> connection, AutoRef<Communication> communication) {
		this->connection = connection;
		this->communication = communication;
	}

	void ConnectionThread::onTask() {
		try {
			connectionTask();
		} catch (NullException & e) {
			logger->loge(e.toString());
		}
	}

	AutoRef<Connection> ConnectionThread::getConnection() {
		return connection;
	}

	void ConnectionThread::connectionTask() {
		connection->registerSelector(selector, Selector::READ | Selector::WRITE);
		try {
			connection->negotiate();
            communication->onConnected(connection);
            while (!interrupted() && !connection->isTerminateFlaged()) {
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
            logger->loge(e.toString());
        }
        
        // notify disconnection to connection manager
        communication->onDisconnected(connection);

		connection->unregisterSelector(selector, Selector::READ | Selector::WRITE);
        connection->close();
        connection->completed() = true;
        
		communication = NULL;
	}

	void ConnectionThread::testReceiveTimeout() {
		if (connection->expiredRecvTimeout()) {
			throw IOException("recv timeout");
		}
	}

	bool ConnectionThread::doReceive() {
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

	bool ConnectionThread::doWrite() {
		bool proc = false;
		if (connection->isWritable(selector)) {
			if (communication->onWriteable(connection)) {
				proc = true;
			}
		}
		return proc;
	}

	bool ConnectionThread::closing() {
		return (connection->isClosed() || communication->isCommunicationCompleted());
	}

	/**
	 * @brief ConnectionThreadInstanceCreator
	 */

	class ConnectionThreadInstanceCreator : public UTIL::InstanceCreator<UTIL::StatefulThread*> {
	private:
	public:
		ConnectionThreadInstanceCreator() {
		}
		virtual ~ConnectionThreadInstanceCreator() {
		}
		virtual UTIL::StatefulThread * createInstance() {
			return new ConnectionThread;
		}

		virtual void releaseInstance(UTIL::StatefulThread * inst) {
			delete inst;
		}
	};

	static ConnectionThreadInstanceCreator instanceCreator;



	/**
	 * @brief ConnectionThreadPool
	 */

	ConnectionThreadPool::ConnectionThreadPool(size_t maxThreads) : ThreadPool(maxThreads, instanceCreator)  {
	}

	ConnectionThreadPool::~ConnectionThreadPool() {
	}

	void ConnectionThreadPool::createConnection(AutoRef<Communication> communication, AutoRef<Connection> connection) {
		ConnectionThread * thread = (ConnectionThread *)acquire();
		if (thread == NULL) {
			throw Exception("ConnectionThreadPool error - no thread available");
		}
		thread->setConnection(connection, communication);
		enqueue(thread);
	}
}
