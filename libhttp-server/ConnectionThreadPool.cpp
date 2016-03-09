#include "ConnectionThreadPool.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();


	/**
	 * @brief ConnectionThread
	 */


	ConnectionThread::ConnectionThread() : FlaggableThread(false), connection(NULL), communication(NULL) {
	}
	ConnectionThread::~ConnectionThread() {
	}

	void ConnectionThread::setConnection(Connection * connection, Communication * communication) {
		this->connection = connection;
		this->communication = communication;
	}

	void ConnectionThread::run() {

		while (!interrupted()) {

			if (!flagged()) {
				idle(10);
				continue;
			}

			try {
				connectionTask();
			} catch (NullException e) {
				logger.loge(e.getMessage());
			}

			setFlag(false);
		}
	}


	void ConnectionThread::connectionTask() {

		connection->registerSelector(selector);

		try {
            communication->onConnected(*connection);
            while (!interrupted() && !connection->isTerminateSignaled()) {
                
                if (connection->isSelectable()) {
                    if (selector.select(1000) > 0) {
                        
                        if (connection->isReadableSelected(selector)) {
                            communication->onReceivable(*connection);
                        }
                        
                        if (connection->isWritableSelected(selector)) {
                            communication->onWriteable(*connection);
                        }
                    }
                } else {
                    communication->onReceivable(*connection);
                    communication->onWriteable(*connection);
                }

                if (connection->isClosed() || communication->isCommunicationCompleted()) {
                    break;
                }
            }
            
        } catch (IOException e) {
            logger.loge(e.getMessage());
        }

		connection->unregisterSelector(selector);

        connection->close();
        
        // notify disconnection to connection manager
        communication->onDisconnected(*connection);
        connection->setCompleted();
        
        delete communication;
		communication = NULL;
	}



	/**
	 * @brief ConnectionThreadInstanceCreator
	 */

	class ConnectionThreadInstanceCreator : public UTIL::InstanceCreator<UTIL::FlaggableThread*> {
	private:
	public:
		ConnectionThreadInstanceCreator() {
		}
		virtual ~ConnectionThreadInstanceCreator() {
		}
		virtual UTIL::FlaggableThread * createInstance() {
			return new ConnectionThread;
		}

		virtual void releaseInstance(UTIL::FlaggableThread * inst) {
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

	void ConnectionThreadPool::createConnection(Communication * communication, Connection * connection) {
		ConnectionThread * thread = (ConnectionThread *)acquire();
		if (thread) {
			thread->setConnection(connection, communication);
			enqueue(thread);
		}
	}

}
