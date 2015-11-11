#include "MultiConnThreadedServer.hpp"

#include <vector>
#include <map>
#include <liboslayer/os.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;

	/**
	 * @brief client thread
	 */
	ClientHandlerThread::ClientHandlerThread(MultiConnThreadedServer & server, ClientSession & client)
		: server(server), client(client) {
	}
    
	ClientHandlerThread::~ClientHandlerThread() {
	}
    
	void ClientHandlerThread::run() {
		Socket * socket = client.getSocket();
		char buffer[1024] = {0,};
		int len = 0;
        try {
            while (!interrupted() && (len = socket->recv(buffer, client.getBufferSize())) > 0) {
                Packet packet(buffer, len);
                server.onClientReceive(client, packet);
            }
        } catch (IOException e) {
        }
		server.onClientDisconnect(client);
	}
    
	ClientSession & ClientHandlerThread::getClient() {
		return client;
	}
    
	void ClientHandlerThread::quit() {
        this->interrupt();
		client.getSocket()->close();
	}
	

	/**
	 * @brief multi conn threaded server
	 */
	MultiConnThreadedServer::MultiConnThreadedServer(int port) : port(port), clientsLock(1) {
	}
    
	MultiConnThreadedServer::~MultiConnThreadedServer() {
	}
	
	void MultiConnThreadedServer::start() {

		server = new ServerSocket(port);
		server->setReuseAddr();
		server->bind();
		server->listen(5);

		server->registerSelector(selector);

		clients.clear();
	}
    
	void MultiConnThreadedServer::poll(unsigned long timeout_milli) {
		if (selector.select(timeout_milli) > 0) {
            listen();
		}

		releaseInvalidThreads();
	}
    
    void MultiConnThreadedServer::listen() {
        
        if (selector.isSelected(server->getFd())) {
            Socket * client = server->accept();
            
            if (client) {
                ClientSession * session = new ClientSession(client, 1024);
                onClientConnect(*session);
            }
        }
        
    }
    
	void MultiConnThreadedServer::stop() {
        
        vector<ClientHandlerThread*> threads;
        
        clientsLock.wait();
		for (map<int, ClientHandlerThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
            threads.push_back(iter->second);
		}
        clientsLock.post();
        
        for (size_t i = 0; i < threads.size(); i++) {
            threads[i]->quit();
        }
        
        for (size_t i = 0; i < threads.size(); i++) {
            threads[i]->join();
        }
		
		clients.clear();

		server->close();
		server = NULL;
	}
    
	bool MultiConnThreadedServer::isRunning() {
		return server != NULL;
	}
	
	bool MultiConnThreadedServer::isClientDisconnected(ClientSession & client) {
		for (map<int, ClientHandlerThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			if (&(iter->second->getClient()) == &client) {
				return false;
			}
		}
		return true;
	}

	void MultiConnThreadedServer::releaseInvalidThreads() {
		clientsLock.wait();
		map<int, ClientHandlerThread*>::iterator iter = clients.begin();
		while (iter != clients.end()) {
			ClientHandlerThread * thread = iter->second;
			if (!thread) {
				clients.erase(iter++);
			} else if (!thread->isRunning()) {
				onClientDisconnect(thread->getClient());
				delete thread;
				clients.erase(iter++);
			} else {
				++iter;
			}
		}
		clientsLock.post();
	}
	
	void MultiConnThreadedServer::onClientConnect(ClientSession & client) {
		ClientHandlerThread * thread = new ClientHandlerThread(*this, client);

		clientsLock.wait();
		clients[client.getId()] = thread;
		clientsLock.post();

		thread->start();
		MultiConn::onClientConnect(client);
	}
    
	void MultiConnThreadedServer::onClientReceive(ClientSession & client, Packet & packet) {
		MultiConn::onClientReceive(client, packet);
	}
    
	void MultiConnThreadedServer::onClientDisconnect(ClientSession & client) {
		int id = client.getId();
		
		MultiConn::onClientDisconnect(client);

		clientsLock.wait();
		clients.erase(id);
		clientsLock.post();

		delete &client;
	}
}