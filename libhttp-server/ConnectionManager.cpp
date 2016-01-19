#include "ConnectionManager.hpp"

#include <liboslayer/Logger.hpp>

namespace HTTP {
    
    using namespace std;
    using namespace OS;
    using namespace UTIL;
    
    static const Logger & logger = LoggerFactory::getDefaultLogger();
    
	/**
	 * @brief ConnectionManager
	 */

    ConnectionManager::ConnectionManager(CommunicationMaker & communicationMaker)
		: serverSocket(NULL), connectionsLock(1), communicationMaker(communicationMaker), threadPool(20) {
            serverSocketMaker = new DefaultServerSocketMaker;
    }
    
    ConnectionManager::ConnectionManager(CommunicationMaker & communicationMaker, ServerSocketMaker * serverSocketMaker)
    : serverSocket(NULL), connectionsLock(1), communicationMaker(communicationMaker), threadPool(20), serverSocketMaker(serverSocketMaker) {
    }

    ConnectionManager::~ConnectionManager() {
        stop();
        if (serverSocketMaker) {
            delete serverSocketMaker;
        }
    }
    
    Connection * ConnectionManager::makeConnection(Socket & client) {
        return new Connection(client);
    }
    
    void ConnectionManager::removeConnection(Connection * connection) {
        delete connection;
    }
    
    void ConnectionManager::onConnect(Socket & client) {
        Connection * connection = makeConnection(client);
        Communication * communication = communicationMaker.makeCommunication();
        
        connectionsLock.wait();
        connectionTable[connection->getId()] = connection;
        connectionsLock.post();
        
		startCommunication(communication, connection);
    }
    
    void ConnectionManager::onDisconnect(Connection * connection) {
        connectionsLock.wait();
        int id = connection->getId();
        if (connectionTable.find(id) != connectionTable.end()) {
            removeConnection(connection);
            connectionTable.erase(id);
        }
        
        connectionsLock.post();
    }
    
    void ConnectionManager::clearConnections() {
        connectionsLock.wait();
        for (map<int, Connection*>::const_iterator iter = connectionTable.begin(); iter != connectionTable.end(); iter++) {
            Connection * connection = iter->second;
            
            connection->signalTerminate();
            while (!connection->isCompleted()) {
                idle(10);
            }
            
            removeConnection(connection);
        }
        connectionTable.clear();
        connectionsLock.post();
    }
    
    void ConnectionManager::start(int port) {
        if (serverSocket) {
            return;
        }
        
        // serverSocket = new ServerSocket(port);
        serverSocket = serverSocketMaker->makeServerSocket(port);
        serverSocket->setReuseAddr(true);
        serverSocket->bind();
        serverSocket->listen(5);
        
        serverSocket->registerSelector(selector);

		threadPool.start();
    }
    
    void ConnectionManager::poll(unsigned long timeout) {
        if (selector.select(timeout) > 0) {
            if (selector.isReadableSelected(*serverSocket)) {
                Socket * client = serverSocket->accept();
                if (client) {
                    onConnect(*client);
                }
            }
        }
        
        removeCompletedConnections();
        removeCompletedThreads();
    }

	void ConnectionManager::removeCompletedConnections() {
        connectionsLock.wait();
        for (map<int, Connection*>::iterator iter = connectionTable.begin(); iter != connectionTable.end();) {
            Connection * connection = iter->second;
            if (connection->isCompleted()) {
                removeConnection(connection);
                connectionTable.erase(iter++);
            } else {
                iter++;
            }
        }
        connectionsLock.post();
    }
    
    void ConnectionManager::removeCompletedThreads() {
		threadPool.collectUnflaggedThreads();
    }
    
    void ConnectionManager::stop() {
        if (!serverSocket) {
            return;
        }
        
        clearConnections();
        
        stopAllThreads();
        
        serverSocket->unregisterSelector(selector);
        serverSocket->close();
        
        serverSocket = NULL;
    }
    
    void ConnectionManager::stopAllThreads() {
		threadPool.stop();
    }

	void ConnectionManager::startCommunication(Communication * communication, Connection * connection) {
		threadPool.createConnection(communication, connection);
	}
}
