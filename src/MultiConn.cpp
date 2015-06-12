#include "MultiConn.hpp"

namespace HTTP {
	
	using namespace std;
	using namespace OS;


	Packet::Packet(int size) : size(size), length(0) {
		buffer = (char*)malloc(size);
		memset(buffer, 0, size);
	}
	Packet::Packet(char * buffer, int size) : size(size), length(0) {
		this->buffer = (char*)malloc(size);
		memcpy(this->buffer, buffer, size);
	}
	Packet::~Packet() {
		if (buffer) {
			free(buffer);
		}
	}
	char * Packet::getBuffer() {
		return buffer;
	}
	int Packet::put(char * data, int len) {
		int cap = size - length;
		if (cap < len) {
			return -1;
		}

		memcpy(buffer + length, data, len);
		length += len;

		return length;
	}
	int Packet::getSize() {
		return size;
	}
	void Packet::resize(int size) {
		if (buffer) {
			free(buffer);
		}
		this->size = size;
		buffer = (char*)malloc(size);
		memset(buffer, 0, size);
	}
	int Packet::getLength() {
		return length;
	}




	MultiConnServer::MultiConnServer(int port)
		: port(port),
		  server(NULL),
		  onConnectListener(NULL),
		  onReceiveListener(NULL),
		  onDisconnectListener(NULL) {
	}
	MultiConnServer::~MultiConnServer() {
	}

	void MultiConnServer::start() {
		server = new ServerSocket(port);
		server->setReuseAddr();
		server->bind();
		server->listen(5);

		server->registerSelector(selector);

		clients.clear();
	}
	void MultiConnServer::poll(unsigned long timeout_milli) {
		
		if (selector.select(timeout_milli) > 0) {
			
			vector<int> selected = selector.getSelected();
			for (size_t i = 0; i < selected.size(); i++) {
					
				int fd = selected[i];

				if (server->compareFd(fd)) {
					Socket * client = server->accept();
					
					if (client) {
						onConnect(*client);
					}
				} else {
					Socket * client = clients[fd];
					if (client) {
						char buffer[1024] = {0,};
						int len = client->recv(buffer, sizeof(buffer));
						if (len <= 0) {
							onDisconnect(*client);
						} else {
							Packet packet(buffer, len);
							onReceive(*client, packet);
						}
					}
				}
			}
		}
	}
	void MultiConnServer::stop() {
		for (size_t i = 0; i < clients.size(); i++) {
			delete clients[i];
		}
		clients.clear();

		server->close();
		server = NULL;
	}
	bool MultiConnServer::isRunning() {
		return server != NULL;
	}

	void MultiConnServer::onConnect(Socket & client) {

		clients[client.getFd()] = &client;
		client.registerSelector(selector);
		
		if (onConnectListener) {
			onConnectListener->onConnect(client);
		}
	}
	
	void MultiConnServer::onReceive(Socket & client, Packet & packet) {
		if (onReceiveListener) {
			onReceiveListener->onReceive(client, packet);
		}
	}

	void MultiConnServer::onDisconnect(Socket & client) {

		int fd = client.getFd();
		
		if (onDisconnectListener) {
			onDisconnectListener->onDisconnect(client);
		}

		clients.erase(fd);
		selector.unset(fd);
		delete &client;
	}

	void MultiConnServer::setOnConnectListener(OnConnectListener * onConnectListener) {
		this->onConnectListener = onConnectListener;
	}
	
	void MultiConnServer::setOnReceiveListener(OnReceiveListener * onReceiveListener) {
		this->onReceiveListener = onReceiveListener;
	}

	void MultiConnServer::setOnDisconnectListener(OnDisconnectListener * onDisconnectListener) {
		this->onDisconnectListener = onDisconnectListener;
	}
}
