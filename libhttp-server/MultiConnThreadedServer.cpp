#include "MultiConnThreadedServer.hpp"

#include <vector>
#include <map>
#include <liboslayer/os.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();

	/**
	 * @brief client thread
	 */
	ClientHandlerThread::ClientHandlerThread(MultiConnThreadedServer & server, Connection & connection)
		: server(server), connection(connection) {
	}
    
	ClientHandlerThread::~ClientHandlerThread() {
	}
    
	void ClientHandlerThread::run() {
        
		Socket * socket = connection.getSocket();
		Selector selector;
		socket->registerSelector(selector);
		char buffer[1024] = {0,};
        
        try {
            while (!interrupted()) {
                
				if (selector.select(1000) > 0) {
                    
                    if (selector.isReadableSelected(*socket)) {
                        int len = socket->recv(buffer, connection.getBufferSize());
                        if (len <= 0) {
                            break;
                        }
                        
                        Packet packet(buffer, len);
                        server.onClientReceive(connection, packet);
                    }
                    
                    if (selector.isWriteableSelected(*socket)) {
                        server.onClientWriteable(connection);
                    }
					
				}
                
				if (connection.isClosed()) {
					break;
				}
            }
        } catch (IOException e) {
            logger.loge(e.getMessage());
        }
		server.onClientDisconnect(connection);
	}
    
	Connection & ClientHandlerThread::getClient() {
		return connection;
	}
    
	void ClientHandlerThread::quit() {
        this->interrupt();
		connection.close();
	}
	

	/**
	 * @brief multi conn threaded server
	 */
	MultiConnThreadedServer::MultiConnThreadedServer(int port) : port(port), clientThreadsLock(1), server(NULL) {
	}
    
	MultiConnThreadedServer::~MultiConnThreadedServer() {
		stop();
	}
	
	void MultiConnThreadedServer::start() {

		if (server) {
			return;
		}

		server = new ServerSocket(port);
		server->setReuseAddr();
		server->bind();
		server->listen(5);

		server->registerSelector(selector);

		clientThreads.clear();
	}
    
	void MultiConnThreadedServer::poll(unsigned long timeout_milli) {
		if (selector.select(timeout_milli) > 0) {
            listen();
		}

		releaseInvalidThreads();
	}

	void MultiConnThreadedServer::releaseInvalidThreads() {
		clientThreadsLock.wait();
		map<int, ClientHandlerThread*>::iterator iter = clientThreads.begin();
		while (iter != clientThreads.end()) {
			ClientHandlerThread * thread = iter->second;
			if (!thread) {
				clientThreads.erase(iter++);
			} else if (!thread->isRunning()) {
				delete thread;
				clientThreads.erase(iter++);
			} else {
				++iter;
			}
		}
		clientThreadsLock.post();
	}
    
    void MultiConnThreadedServer::listen() {
        
        if (selector.isSelected(server->getFd())) {
            Socket * client = server->accept();
            
            if (client) {
                Connection * session = new Connection(client, 1024);
                onClientConnect(*session);
            }
        }
    }
	
	void MultiConnThreadedServer::onClientConnect(Connection & connection) {
		ClientHandlerThread * thread = new ClientHandlerThread(*this, connection);

		clientThreadsLock.wait();
		clientThreads[connection.getId()] = thread;
		clientThreadsLock.post();

		thread->start();
		MultiConn::onClientConnect(connection);
	}
    
	void MultiConnThreadedServer::onClientReceive(Connection & connection, Packet & packet) {
		MultiConn::onClientReceive(connection, packet);
	}
    
	void MultiConnThreadedServer::onClientDisconnect(Connection & connection) {
		int id = connection.getId();
		
		MultiConn::onClientDisconnect(connection);

		clientThreadsLock.wait();
		clientThreads.erase(id);
		clientThreadsLock.post();

		delete &connection;
	}

	void MultiConnThreadedServer::stop() {

		if (!server) {
			return;
		}
        
        vector<ClientHandlerThread*> threads;
        
        clientThreadsLock.wait();
		for (map<int, ClientHandlerThread*>::iterator iter = clientThreads.begin(); iter != clientThreads.end(); iter++) {
            threads.push_back(iter->second);
		}
        clientThreadsLock.post();
        
        for (size_t i = 0; i < threads.size(); i++) {
            threads[i]->quit();
        }
        
        for (size_t i = 0; i < threads.size(); i++) {
            threads[i]->join();
        }
		
		clientThreads.clear();

		server->close();
		server = NULL;
	}
    
	bool MultiConnThreadedServer::isRunning() {
		return server != NULL;
	}
}