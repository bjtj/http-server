#include "ConnectionManager.hpp"
#include <liboslayer/Logger.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {
    
    using namespace std;
    using namespace OS;
    using namespace UTIL;
    
    static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

    ConnectionManager::ConnectionManager(AutoRef<CommunicationMaker> communicationMaker,
										 size_t threadCount) :
		connectionsLock(1),
		communicationMaker(communicationMaker),
		threadPool(threadCount),
		recvTimeout(0)
	{
		serverSocketMaker = AutoRef<ServerSocketMaker>(new DefaultServerSocketMaker);
		threadPool.addObserver(this);
    }
    
    ConnectionManager::ConnectionManager(AutoRef<CommunicationMaker> communicationMaker,
										 size_t threadCount,
										 AutoRef<ServerSocketMaker> serverSocketMaker) :
		serverSocketMaker(serverSocketMaker),
		connectionsLock(1),
		communicationMaker(communicationMaker),
		threadPool(threadCount),
		recvTimeout(0)
	{
		threadPool.addObserver(this);
    }

    ConnectionManager::~ConnectionManager() {
        stop();
    }

	void ConnectionManager::start(int port) {
		start(port, 5);
	}
    
    void ConnectionManager::start(int port, int backlog) {
        if (!serverSocket.nil()) {
            return;
        }
        serverSocket = serverSocketMaker->makeServerSocket(port);
        serverSocket->setReuseAddr(true);
        serverSocket->bind();
        serverSocket->listen(backlog);
        serverSocket->registerSelector(selector, Selector::READ);
		threadPool.start();
    }

	void ConnectionManager::stop() {
        if (serverSocket.nil()) {
            return;
        }
        clearConnections();
        stopAllThreads();
        serverSocket->unregisterSelector(selector, Selector::READ);
        serverSocket->close();
        serverSocket = NULL;
    }
    
    void ConnectionManager::poll(unsigned long timeout) {
        if (selector.select(timeout) > 0) {
            if (selector.isReadable(*serverSocket)) {
				try {
					AutoRef<Socket> client(serverSocket->accept());
					if (!client.nil()) {
						onConnect(client);
					}
				} catch (IOException & e) {
					logger->loge("client connection handling failed with '" + e.getMessage() + "'");
				}
            }
        }
    }

	void ConnectionManager::clearConnections() {
        connectionsLock.wait();
        for (map<int, AutoRef<Connection> >::const_iterator iter = connections.begin();
			 iter != connections.end(); iter++)
		{
            AutoRef<Connection> connection = iter->second;
            connection->flagTerminate(true);
            while (!connection->completed()) {
                idle(10);
            }
        }
        connections.clear();
        connectionsLock.post();
    }
    
    void ConnectionManager::onConnect(AutoRef<Socket> client) {
        AutoRef<Connection> connection = makeConnection(client);
		connection->setRecvTimeout(recvTimeout);
        AutoRef<Communication> communication = communicationMaker->makeCommunication();
		registerConnection(connection);
		try {
			startCommunication(communication, connection);
		} catch (Exception & e) {
			logger->loge(e.getMessage());
			handleMaxCapacity(connection);
			connection->close();
			onDisconnect(connection);
		}
    }

	AutoRef<Connection> ConnectionManager::makeConnection(AutoRef<Socket> client) {
        return AutoRef<Connection>(new Connection(client));
    }
    
    void ConnectionManager::onDisconnect(AutoRef<Connection> connection) {
		unregisterConnection(connection);
    }

	void ConnectionManager::registerConnection(UTIL::AutoRef<Connection> connection) {
		connectionsLock.wait();
        connections[connection->getId()] = connection;
        connectionsLock.post();
	}
	
	void ConnectionManager::unregisterConnection(UTIL::AutoRef<Connection> connection) {
		connectionsLock.wait();
        int id = connection->getId();
        if (connections.find(id) != connections.end()) {
            connections.erase(id);
        }
        connectionsLock.post();
	}

	void ConnectionManager::removeCompletedConnections() {
        connectionsLock.wait();
        for (map<int, AutoRef<Connection> >::iterator iter = connections.begin(); iter != connections.end();) {
            AutoRef<Connection> connection = iter->second;
            if (connection->completed()) {
				connections.erase(iter++);
            } else {
                iter++;
            }
        }
        connectionsLock.post();
    }
    
    void ConnectionManager::stopAllThreads() {
		threadPool.stop();
    }

	void ConnectionManager::startCommunication(AutoRef<Communication> communication, AutoRef<Connection> connection) {
		threadPool.createConnection(communication, connection);
	}

	size_t ConnectionManager::getConnectionCount() {
		connectionsLock.wait();
		size_t cnt = connections.size();
		connectionsLock.post();
		return cnt;
	}

	void ConnectionManager::onUpdate(Observable * target) {
		ConnectionThread * t = (ConnectionThread*)target;
		onDisconnect(t->getConnection());
	}

	void ConnectionManager::handleMaxCapacity(AutoRef<Connection> connection) {
		if (!onMaxCapacity.nil()) {
			onMaxCapacity->onMaxCapacity(*this, connection);
		}
	}

	void ConnectionManager::setOnMaxCapacity(AutoRef<OnMaxCapacity> onMaxCapacity) {
		this->onMaxCapacity = onMaxCapacity;
	}

	void ConnectionManager::setRecvTimeout(unsigned long recvTimeout) {
		this->recvTimeout = recvTimeout;
	}
	
	unsigned long ConnectionManager::getRecvTimeout() {
		return recvTimeout;
	}

	size_t ConnectionManager::available() {
		return threadPool.freeCount();
	}
	
	size_t ConnectionManager::working() {
		return threadPool.workingCount();
	}
	
	size_t ConnectionManager::capacity() {
		return threadPool.capacity();
	}
    
    vector<AutoRef<Connection> > ConnectionManager::getConnectionList() {
        vector<AutoRef<Connection> > lst;
        connectionsLock.wait();
        for (map<int, AutoRef<Connection> >::iterator iter = connections.begin(); iter != connections.end(); iter++) {
            lst.push_back(iter->second);
        }
        connectionsLock.post();
        return lst;
    }
}
