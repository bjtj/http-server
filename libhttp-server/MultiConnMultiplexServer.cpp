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
        vector<int> selected = selector.getSelected();
        for (size_t i = 0; i < selected.size(); i++) {
            
            int fd = selected[i];
            
            if (server->compareFd(fd)) {
                Socket * client = server->accept();
                
                if (client) {
                    ClientSession * session = new ClientSession(client, 1024);
                    onClientConnect(*session);
                }
            } else {
                ClientSession * client = clients[fd];
                if (client) {
                    char buffer[1024] = {0,};
                    int len = client->getSocket()->recv(buffer, client->getBufferSize());
                    if (len <= 0) {
                        onClientDisconnect(*client);
                    } else {
                        Packet packet(buffer, len);
                        onClientReceive(*client, packet);
                        if (client->isClosed()) {
                            onClientDisconnect(*client);
                        }
                    }
                }
            }
        }
    }
    
	void MultiConnMultiplexServer::stop() {

		for (map<int, ClientSession*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			onClientDisconnect(*(iter->second));
		}
		
		clients.clear();

		server->close();
		server = NULL;
	}
    
	bool MultiConnMultiplexServer::isRunning() {
		return server != NULL;
	}

	bool MultiConnMultiplexServer::isClientDisconnected(ClientSession & client) {
        
		for (size_t i = 0; i < clients.size(); i++) {
			if (clients[(int)i] == &client) {
				return false;
			}
		}
		return true;
	}

	void MultiConnMultiplexServer::onClientConnect(ClientSession & client) {
		Socket * socket = client.getSocket();
		clients[client.getId()] = &client;
		socket->registerSelector(selector);
		
		MultiConn::onClientConnect(client);
	}
	
	void MultiConnMultiplexServer::onClientReceive(ClientSession & client, Packet & packet) {
		MultiConn::onClientReceive(client, packet);
	}

	void MultiConnMultiplexServer::onClientDisconnect(ClientSession & client) {
		int fd = client.getId();

		MultiConn::onClientDisconnect(client);
		
		clients.erase(fd);
		selector.unset(fd);
		client.close();
		delete &client;
	}
}