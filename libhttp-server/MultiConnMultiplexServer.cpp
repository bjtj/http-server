#include "MultiConnMultiplexServer.hpp"

#include <vector>
#include <map>
#include <liboslayer/os.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;

	/**
	 * @brief multi conn multiplex server
	 */
	MultiConnMultiplexServer::MultiConnMultiplexServer(int port)
		: port(port),
		  server(NULL) {
	}
    
	MultiConnMultiplexServer::~MultiConnMultiplexServer() {
	}

	void MultiConnMultiplexServer::start() {
        
		server = new ServerSocket(port);
		server->setReuseAddr();
		server->bind();
		server->listen(5);

		server->registerSelector(selector);

		clients.clear();
	}
    
	void MultiConnMultiplexServer::poll(unsigned long timeout_milli) {
		
		if (selector.select(timeout_milli) > 0) {
            listen();
		}
	}
    
    void MultiConnMultiplexServer::listen() {
        
        vector<Selection> selected = selector.getSelected();
        for (size_t i = 0; i < selected.size(); i++) {

            Selection selection = selected[i];
            int fd = selection.getFd();
            
            if (server->compareFd(fd)) {
                Socket * client = server->accept();
                
                if (client) {
                    Connection * session = new Connection(client, 1024);
                    onClientConnect(*session);
                }
            } else {
                Connection * connection = clients[fd];
                if (connection) {
                    
                    if (selection.isReadable()) {
                        char buffer[1024] = {0,};
                        
                        int len = connection->getSocket()->recv(buffer, connection->getBufferSize());
                        if (len <= 0) {
                            onClientDisconnect(*connection);
                        } else {
                            Packet packet(buffer, len);
                            onClientReceive(*connection, packet);
                            if (connection->isClosed()) {
                                onClientDisconnect(*connection);
                            }
                        }
                    }
                    
                    if (selection.isWritable()) {
                        onClientWriteable(*connection);
                    }
                }
            }
        }
    }
    
	void MultiConnMultiplexServer::stop() {

		for (map<int, Connection*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			onClientDisconnect(*(iter->second));
		}
		
		clients.clear();

		server->close();
		server = NULL;
	}
    
	bool MultiConnMultiplexServer::isRunning() {
		return server != NULL;
	}
	
	void MultiConnMultiplexServer::onClientConnect(Connection & connection) {
		Socket * socket = connection.getSocket();
		clients[connection.getId()] = &connection;
		socket->registerSelector(selector);
		
		MultiConn::onClientConnect(connection);
	}
	
	void MultiConnMultiplexServer::onClientReceive(Connection & connection, Packet & packet) {
		MultiConn::onClientReceive(connection, packet);
	}

	void MultiConnMultiplexServer::onClientDisconnect(Connection & connection) {
		int fd = connection.getId();

		MultiConn::onClientDisconnect(connection);
		
		clients.erase(fd);
		selector.unset(fd);
		connection.close();
		delete &connection;
	}
}