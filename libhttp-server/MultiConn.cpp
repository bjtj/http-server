#include "MultiConn.hpp"

namespace HTTP {
	
	using namespace std;
	using namespace OS;

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
