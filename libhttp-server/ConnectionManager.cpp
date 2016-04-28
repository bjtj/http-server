#include "ConnectionManager.hpp"
#include <liboslayer/Logger.hpp>

namespace HTTP {
    
    using namespace std;
    using namespace OS;
    using namespace UTIL;
    
    static AutoRef<Logger> logger = LoggerFactory::getInstance().getLogger(__FILE__);
    
	/**
	 * @brief ConnectionManager
	 */

    ConnectionManager::ConnectionManager(AutoRef<CommunicationMaker> communicationMaker, size_t threadCount) :
		connectionsLock(1),
		communicationMaker(communicationMaker),
		threadPool(threadCount) {
		
		serverSocketMaker = AutoRef<ServerSocketMaker>(new DefaultServerSocketMaker);

		threadPool.addObserver(this);
    }
    
    ConnectionManager::ConnectionManager(AutoRef<CommunicationMaker> communicationMaker, size_t threadCount, AutoRef<ServerSocketMaker> serverSocketMaker) :
		serverSocketMaker(serverSocketMaker),
		connectionsLock(1),
		communicationMaker(communicationMaker),
		threadPool(threadCount) {

		threadPool.addObserver(this);
    }

    ConnectionManager::~ConnectionManager() {
        stop();
    }
    
    AutoRef<Connection> ConnectionManager::makeConnection(AutoRef<Socket> client) {
        return AutoRef<Connection>(new Connection(client));
    }
    
    void ConnectionManager::onConnect(AutoRef<Socket> client) {
        AutoRef<Connection> connection = makeConnection(client);
        AutoRef<Communication> communication = communicationMaker->makeCommunication();

        connectionsLock.wait();
        connectionTable[connection->getId()] = connection;
        connectionsLock.post();

		try {
			startCommunication(communication, connection);
		} catch (Exception e) {
			logger->loge(e.getMessage());

			// TODO: handle thread pool full e.g. 500 not available
			
			connection->close();
			onDisconnect(connection);
		}
    }
    
    void ConnectionManager::onDisconnect(AutoRef<Connection> connection) {
        connectionsLock.wait();
        int id = connection->getId();
        if (connectionTable.find(id) != connectionTable.end()) {
            connectionTable.erase(id);
        }
        connectionsLock.post();
    }
    
    void ConnectionManager::clearConnections() {
        connectionsLock.wait();
        for (map<int, AutoRef<Connection> >::const_iterator iter = connectionTable.begin(); iter != connectionTable.end(); iter++) {
            AutoRef<Connection> connection = iter->second;
            connection->signalTerminate();
            while (!connection->isCompleted()) {
                idle(10);
            }
        }
        connectionTable.clear();
        connectionsLock.post();
    }
    
    void ConnectionManager::start(int port) {
        if (!serverSocket.nil()) {
            return;
        }
        
        serverSocket = serverSocketMaker->makeServerSocket(port);
        serverSocket->setReuseAddr(true);
        serverSocket->bind();
        serverSocket->listen(5);
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
            if (selector.isReadableSelected(*serverSocket)) {
                AutoRef<Socket> client(serverSocket->accept());
                if (!client.nil()) {
                    onConnect(client);
                }
            }
        }
    }

	void ConnectionManager::removeCompletedConnections() {
        connectionsLock.wait();
        for (map<int, AutoRef<Connection> >::iterator iter = connectionTable.begin(); iter != connectionTable.end();) {
            AutoRef<Connection> connection = iter->second;
            if (connection->isCompleted()) {
				connectionTable.erase(iter++);
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
		size_t cnt = connectionTable.size();
		connectionsLock.post();
		return cnt;
	}

	void ConnectionManager::update(Observable * target) {
		ConnectionThread * t = (ConnectionThread*)target;
		onDisconnect(t->getConnection());
	}
}
