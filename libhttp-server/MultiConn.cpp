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

	/**
	 * @brief client session
	 */
	ClientSession::ClientSession(OS::Socket * socket, int maxBufferSize)
		: socket(socket), maxBufferSize(maxBufferSize), bufferSize(maxBufferSize) {
	}
	
	ClientSession::~ClientSession() {
	}

	int ClientSession::getId() {
		return socket->getFd();
	}
	
	Socket * ClientSession::getSocket() {
		return socket;
	}
	
	void ClientSession::setBufferSize(int bufferSize) {
		this->bufferSize = bufferSize;
	}
	
	int ClientSession::getBufferSize() {
		return bufferSize;
	}
	
	int ClientSession::getMaxBufferSize() {
		return maxBufferSize;
	}
	
	bool ClientSession::operator==(const ClientSession &other) const {
		return (this->socket == other.socket);
	}
	

	/**
	 * @brief multi connectino constructor
	 */
	MultiConn::MultiConn() : onConnectListener(NULL), onReceiveListener(NULL), onDisconnectListener(NULL) {
	}
	MultiConn::~MultiConn() {
	}
	
	void MultiConn::onConnect(ClientSession & client) {
		if (onConnectListener) {
			onConnectListener->onConnect(*this, client);
		}
	}
	void MultiConn::onReceive(ClientSession & client, Packet & packet) {
		if (onReceiveListener) {
			onReceiveListener->onReceive(*this, client, packet);
		}
	}
	void MultiConn::onDisconnect(ClientSession & client) {
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
			
			vector<int> selected = selector.getSelected();
			for (size_t i = 0; i < selected.size(); i++) {
					
				int fd = selected[i];

				if (server->compareFd(fd)) {
					Socket * client = server->accept();
					
					if (client) {
						ClientSession * session = new ClientSession(client, 1024);
						onConnect(*session);
					}
				} else {
					ClientSession * client = clients[fd];
					if (client) {
						char buffer[1024] = {0,};
						int len = client->getSocket()->recv(buffer, client->getBufferSize());
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
		vector<ClientSession*> vec;

		for (map<int, ClientSession*>::iterator iter = clients.begin();
			 iter != clients.end(); iter++) {
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

	bool MultiConnMultiplexServer::isDisconnected(ClientSession & client) {
		for (size_t i = 0; i < clients.size(); i++) {
			if (clients[i] == &client) {
				return false;
			}
		}
		return true;
	}
	
	void MultiConnMultiplexServer::disconnect(ClientSession & client) {
		onDisconnect(client);
	}

	void MultiConnMultiplexServer::onConnect(ClientSession & client) {
		Socket * socket = client.getSocket();
		clients[client.getId()] = &client;
		socket->registerSelector(selector);
		
		MultiConn::onConnect(client);
	}
	
	void MultiConnMultiplexServer::onReceive(ClientSession & client, Packet & packet) {
		
		MultiConn::onReceive(client, packet);
	}

	void MultiConnMultiplexServer::onDisconnect(ClientSession & client) {

		Socket * socket = client.getSocket();
		int fd = client.getId();

		MultiConn::onDisconnect(client);
		
		clients.erase(fd);
		selector.unset(fd);
		socket->close();
		delete socket;
		delete &client;
	}


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
		while ((len = socket->recv(buffer, client.getBufferSize())) > 0) {
			Packet packet(buffer, len);
			server.onReceive(client, packet);
		}
		server.onDisconnect(client);
	}
	ClientSession & ClientHandlerThread::getClient() {
		return client;
	}
	void ClientHandlerThread::quit() {
		client.getSocket()->close();
	}
	

	/**
	 * @brief multi conn threaded server
	 */
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
						ClientSession * session = new ClientSession(client, 1024);
						onConnect(*session);
					}
					break;
				}
			}
		}

		releaseInvalidThreads();
	}
	void MultiConnThreadedServer::stop() {
		vector<ClientHandlerThread*> vec;

		for (map<int, ClientHandlerThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			vec.push_back(iter->second);
		}
		
		for (size_t i = 0; i < vec.size(); i++) {
			disconnect(vec[i]->getClient());
		}
		
		clients.clear();

		server->close();
		server = NULL;
	}
	bool MultiConnThreadedServer::isRunning() {
		return server != NULL;
	}
	
	bool MultiConnThreadedServer::isDisconnected(ClientSession & client) {
		for (map<int, ClientHandlerThread*>::iterator iter = clients.begin(); iter != clients.end(); iter++) {
			if (&(iter->second->getClient()) == &client) {
				return false;
			}
		}
		return true;
	}
	void MultiConnThreadedServer::disconnect(ClientSession & client) {
		onDisconnect(client);
	}

	void MultiConnThreadedServer::releaseInvalidThreads() {
		map<int, ClientHandlerThread*>::iterator iter = clients.begin();
		while (iter != clients.end()) {
			ClientHandlerThread * thread = iter->second;
			if (!thread) {
				clients.erase(iter++);
			} else if (!thread->isRunning()) {
				disconnect(thread->getClient());
				delete thread;
				clients.erase(iter++);
			} else {
				++iter;
			}
		}
	}
	
	void MultiConnThreadedServer::onConnect(ClientSession & client) {
		ClientHandlerThread * thread = new ClientHandlerThread(*this, client);
		clients[client.getId()] = thread;
		thread->start();
		MultiConn::onConnect(client);
	}
	void MultiConnThreadedServer::onReceive(ClientSession & client, Packet & packet) {
		MultiConn::onReceive(client, packet);
	}
	void MultiConnThreadedServer::onDisconnect(ClientSession & client) {
		int id = client.getId();
		ClientHandlerThread * thread = clients[id];
		
		MultiConn::onDisconnect(client);

		clients.erase(id);
		thread->quit();
		thread->join();
		delete client.getSocket();
		delete &client;
	}
}
