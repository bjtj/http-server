#include "ConnectionManager.hpp"

#include <liboslayer/Logger.hpp>

namespace HTTP {
    
    using namespace std;
    using namespace OS;
    using namespace UTIL;
    
    static const Logger & logger = LoggerFactory::getDefaultLogger();
    

    ConnectionThread::ConnectionThread(Connection & connection, Communication & communication) : connection(connection), communication(communication) {
    }
    ConnectionThread::~ConnectionThread() {
    }
    
    void ConnectionThread::run() {
        
        connection.registerSelector(selector);
        
        try {
            communication.onConnected(connection);
            while (!interrupted() && !connection.isTerminateSignaled()) {
                if (selector.select(1000) > 0) {
                    
                    if (connection.isReadableSelected(selector)) {
                        Packet & packet = connection.read();
                        communication.onDataReceived(connection, packet);
                    }
                    
                    if (connection.isWritableSelected(selector)) {
                        communication.onWriteable(connection);
                    }
                }

                if (connection.isClosed() || communication.isCommunicationCompleted()) {
                    break;
                }
            }
            
        } catch (IOException e) {
            logger.loge(e.getMessage());
        }
        
        connection.close();
        
        // notify disconnection to connection manager
        communication.onDisconnected(connection);
        connection.setCompleted();
        
        delete &communication;
    }


    ConnectionManager::ConnectionManager(CommunicationMaker & communicationMaker) : serverSocket(NULL), connectionsLock(1), communicationMaker(communicationMaker) {
    }

    ConnectionManager::~ConnectionManager() {
        stop();
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
        
        ConnectionThread * thread = new ConnectionThread(*connection, *communication);
        threads.push_back(thread);
        thread->start();
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
    
    void ConnectionManager::removeCompletedConnections() {
        
        connectionsLock.wait();
        for (map<int, Connection*>::const_iterator iter = connectionTable.begin(); iter != connectionTable.end();) {
            Connection * connection = iter->second;
            if (connection->isCompleted()) {
                removeConnection(connection);
                iter = connectionTable.erase(iter);
            } else {
                iter++;
            }
        }
        connectionsLock.post();
    }
    
    void ConnectionManager::start(int port) {
        if (serverSocket) {
            return;
        }
        
        serverSocket = new ServerSocket(port);
        serverSocket->setReuseAddr();
        serverSocket->bind();
        serverSocket->listen(5);
        
        serverSocket->registerSelector(selector);
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
    
    void ConnectionManager::removeCompletedThreads() {
        for (vector<Thread*>::const_iterator iter = threads.begin(); iter != threads.end();) {
            
            Thread * thread = *iter;
            if (!thread->isRunning()) {
                delete thread;
                iter = threads.erase(iter);
            } else {
                iter++;
            }
        }
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
        for (size_t i = 0; i < threads.size(); i++) {
            Thread * thread = threads[i];
            thread->interrupt();
            thread->join();
            delete thread;
        }
        threads.clear();
    }

}