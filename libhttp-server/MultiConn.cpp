#include "MultiConn.hpp"

namespace HTTP {
	
	using namespace std;
	using namespace OS;

	Packet::Packet() : buffer(NULL), _size(0), _length(0) {
	}

	Packet::Packet(int size) : _size(size), _length(0) {
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
	}
    
	Packet::Packet(char * buffer, int size) : _size(size), _length(_size) {
		if (buffer) {
			this->buffer = (char*)malloc(_size);
			memcpy(this->buffer, buffer, _size);
		} else {
			this->buffer = NULL;
		}
	}

	Packet::Packet(const Packet & other) {
		this->_size = other._size;
		this->_length = other._length;
		if (this->_size > 0) {
			this->buffer = (char*)malloc(this->_size);
			memcpy(this->buffer, other.buffer, this->_size);
		} else {
			this->buffer = NULL;
		}
	}
    
	Packet::~Packet() {
		if (buffer) {
			free(buffer);
		}
	}
    void Packet::clear() {
		if (buffer) {
			memset(buffer, 0, _size);
		}
		_length = 0;
	}
	char * Packet::getBuffer() {
		return buffer;
	}
    
	int Packet::put(char * data, int len) {
		int cap = _size - _length;
		if (!buffer || cap < len) {
			return -1;
		}

		memcpy(buffer + _length, data, len);
		_length += len;

		return _length;
	}
    
	int Packet::size() {
		return _size;
	}
    
	void Packet::resize(int size) {
		if (buffer) {
			free(buffer);
		}
		_size = size;
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
		_length = 0;
	}
    
	int Packet::length() {
		return _length;
	}

	Packet & Packet::operator= (const Packet & other) {
		this->_size = other._size;
		this->_length = other._length;
		if (this->_size > 0) {
			this->buffer = (char*)malloc(this->_size);
			memcpy(this->buffer, other.buffer, this->_size);
		} else {
			this->buffer = NULL;
		}
		return *this;
	}

	/**
	 * @brief client session
	 */
	Connection::Connection(OS::Socket * socket, int maxBufferSize)
		: socket(socket), maxBufferSize(maxBufferSize), bufferSize(maxBufferSize) {
			id = socket->getFd();
	}
	
	Connection::~Connection() {
		if (socket) {
			delete socket;
		}
	}

	int Connection::getId() {
		return id;
	}
	
	Socket * Connection::getSocket() {
		return socket;
	}
	
	void Connection::setBufferSize(int bufferSize) {
		this->bufferSize = bufferSize;
	}
	
	int Connection::getBufferSize() {
		return bufferSize;
	}
	
	int Connection::getMaxBufferSize() {
		return maxBufferSize;
	}

	bool Connection::isClosed() {
		return (socket ? socket->isClosed() : true);
	}

	void Connection::close() {
		if (socket) {
			socket->close();
		}
	}
	
	bool Connection::operator==(const Connection &other) const {
		return (this->socket == other.socket);
	}
	

	/**
	 * @brief multi connectino constructor
	 */
	MultiConn::MultiConn() : onConnectListener(NULL), onReceiveListener(NULL), onDisconnectListener(NULL) {
	}
    
	MultiConn::~MultiConn() {
	}
	
	void MultiConn::onClientConnect(Connection & connection) {
		if (onConnectListener) {
			onConnectListener->onClientConnect(*this, connection);
		}
	}
    
	void MultiConn::onClientReceive(Connection & connection, Packet & packet) {
		if (onReceiveListener) {
			onReceiveListener->onClientReceive(*this, connection, packet);
		}
	}
    
    void MultiConn::onClientWriteable(Connection & connection) {
        if (onWriteableListener) {
            onWriteableListener->onClientWriteable(*this, connection);
        }
    }
    
	void MultiConn::onClientDisconnect(Connection & connection) {
		if (onDisconnectListener) {
			onDisconnectListener->onClientDisconnect(*this, connection);
		}
	}

	void MultiConn::setOnConnectListener(OnConnectListener * onConnectListener) {
		this->onConnectListener = onConnectListener;
	}
    
	void MultiConn::setOnReceiveListener(OnReceiveListener * onReceiveListener) {
		this->onReceiveListener = onReceiveListener;
	}
    
    void MultiConn::setOnWriteableListener(OnWriteableListener * onWriteableListener) {
        this->onWriteableListener = onWriteableListener;
    }
    
	void MultiConn::setOnDisconnectListener(OnDisconnectListener * onDisconnectListener) {
		this->onDisconnectListener = onDisconnectListener;
	}

	void MultiConn::setProtocol(MultiConnProtocol * protocol) {
		setOnConnectListener(protocol);
		setOnReceiveListener(protocol);
        setOnWriteableListener(protocol);
		setOnDisconnectListener(protocol);
	}
	
}
