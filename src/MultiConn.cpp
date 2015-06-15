#include "MultiConn.hpp"

namespace HTTP {
	
	using namespace std;
	using namespace OS;


	Packet::Packet(int size) : _size(size), _length(0) {
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
	}
	Packet::Packet(char * buffer, size_t size) : _size(size), _length(_size) {
		this->buffer = (char*)malloc(_size);
		memcpy(this->buffer, buffer, _size);
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
		int cap = _size - _length;
		if (cap < len) {
			return -1;
		}

		memcpy(buffer + _length, data, len);
		_length += len;

		return _length;
	}
	size_t Packet::size() {
		return _size;
	}
	void Packet::resize(int size) {
		if (buffer) {
			free(buffer);
		}
		_size = size;
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
	}
	int Packet::length() {
		return _length;
	}





	MultiConn::MultiConn() : onConnectListener(NULL), onReceiveListener(NULL), onDisconnectListener(NULL) {
	}
	MultiConn::~MultiConn() {
	}
	
	void MultiConn::onConnect(OS::Socket & client) {
		if (onConnectListener) {
			onConnectListener->onConnect(*this, client);
		}
	}
	void MultiConn::onReceive(OS::Socket & client, Packet & packet) {
		if (onReceiveListener) {
			onReceiveListener->onReceive(*this, client, packet);
		}
	}
	void MultiConn::onDisconnect(OS::Socket & client) {
		if (onDisconnectListener) {
			onDisconnectListener->onDisconnect(*this, client);
		}
	}

	void MultiConn::setOnConnectListener(OnConnectListener * onConnectListener) {
		this->onConnectListener = onConnectListener;
	}
	void MultiConn::setOnReceiveListener(OnReceiveListener * onReceiveListener) {
		this->onReceiveListener = onReceiveListener;
	}
	void MultiConn::setOnDisconnectListener(OnDisconnectListener * onDisconnectListener) {
		this->onDisconnectListener = onDisconnectListener;
	}

	void MultiConn::setProtocol(MultiConnProtocol * protocol) {
		setOnConnectListener(protocol);
		setOnReceiveListener(protocol);
		setOnDisconnectListener(protocol);
	}
	




	

	
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
	void MultiConnMultiplexServer::stop() {
		vector<Socket*> vec;

		for (map<int, Socket*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			vec.push_back(iter->second);
		}
		
		for (size_t i = 0; i < vec.size(); i++) {
			disconnect(*vec[i]);
		}
		
		clients.clear();

		server->close();
		server = NULL;
	}
	bool MultiConnMultiplexServer::isRunning() {
		return server != NULL;
	}

	bool MultiConnMultiplexServer::isDisconnected(Socket & client) {
		for (size_t i = 0; i < clients.size(); i++) {
			if (clients[i] == &client) {
				return false;
			}
		}
		return true;
	}
	
	void MultiConnMultiplexServer::disconnect(Socket & client) {
		onDisconnect(client);
	}

	void MultiConnMultiplexServer::onConnect(Socket & client) {

		clients[client.getFd()] = &client;
		client.registerSelector(selector);
		
		MultiConn::onConnect(client);
	}
	
	void MultiConnMultiplexServer::onReceive(Socket & client, Packet & packet) {
		
		MultiConn::onReceive(client, packet);
	}

	void MultiConnMultiplexServer::onDisconnect(Socket & client) {
		
		int fd = client.getFd();

		MultiConn::onDisconnect(client);
		
		clients.erase(fd);
		selector.unset(fd);
		client.close();
		delete &client;
	}






	ClientThread::ClientThread(MultiConnThreadedServer & server, OS::Socket & socket)
		: server(server), socket(socket) {
	}
	ClientThread::~ClientThread() {
	}
	void ClientThread::run() {
		char buffer[1024] = {0,};
		int len = 0;
		while ((len = socket.recv(buffer, sizeof(buffer))) > 0) {
			Packet packet(buffer, len);
			server.onReceive(socket, packet);
		}
	}
	OS::Socket & ClientThread::getSocket() {
		return socket;
	}
	


	MultiConnThreadedServer::MultiConnThreadedServer(int port) : port(port) {
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
			
			vector<int> selected = selector.getSelected();
			for (size_t i = 0; i < selected.size(); i++) {
					
				int fd = selected[i];

				if (server->compareFd(fd)) {
					Socket * client = server->accept();
					
					if (client) {
						onConnect(*client);
					}
					break;
				}
			}
		}

		releaseInvalidThreads();
	}
	void MultiConnThreadedServer::stop() {
		vector<ClientThread*> vec;

		for (map<int, ClientThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			vec.push_back(iter->second);
		}
		
		for (size_t i = 0; i < vec.size(); i++) {
			disconnect(vec[i]->getSocket());
		}
		
		clients.clear();

		server->close();
		server = NULL;
	}
	bool MultiConnThreadedServer::isRunning() {
		return server != NULL;
	}
	
	bool MultiConnThreadedServer::isDisconnected(OS::Socket & client) {
		for (map<int, ClientThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			if (&(iter->second->getSocket()) == &client) {
				return false;
			}
		}
		return true;
	}
	void MultiConnThreadedServer::disconnect(OS::Socket & client) {
		onDisconnect(client);
		client.close();
	}

	void MultiConnThreadedServer::releaseInvalidThreads() {
		map<int, ClientThread*>::iterator iter = clients.begin();
		while (iter != clients.end()) {
			ClientThread * thread = iter->second;
			if (!thread) {
				clients.erase(iter++);
			} else if (!thread->isRunning()) {
				disconnect(thread->getSocket());
				delete thread;
				clients.erase(iter++);
			} else {
				++iter;
			}
		}
	}
	
	void MultiConnThreadedServer::onConnect(OS::Socket & client) {
		ClientThread * thread = new ClientThread(*this, client);
		clients[client.getFd()] = thread;
		thread->start();
		MultiConn::onConnect(client);
	}
	void MultiConnThreadedServer::onReceive(OS::Socket & client, Packet & packet) {
		MultiConn::onReceive(client, packet);
	}
	void MultiConnThreadedServer::onDisconnect(OS::Socket & client) {
		MultiConn::onDisconnect(client);
	}
}
