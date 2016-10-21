#include "Connection.hpp"

namespace HTTP {
    
	using namespace std;
	using namespace OS;
	using namespace UTIL;

	Connection::Connection(AutoRef<Socket> socket) :
		_socket(socket),
		terminateFlag(false),
		_completed(false),
		_packet(4096),
		id(socket->getFd()),
		_recvTimeout(0)
	{
		/**/
	}
	Connection::Connection(AutoRef<Socket> socket, size_t packetSize) :
		_socket(socket),
		terminateFlag(false),
		_completed(false),
		_packet(packetSize),
		id(socket->getFd()),
		_recvTimeout(0)
	{
		/**/
	}

	Connection::~Connection() {
		/**/
	}

	int Connection::getId() {
		return id;
	}

	void Connection::registerSelector(Selector & selector, unsigned char flags) {
		_socket->registerSelector(selector, flags);
	}

	void Connection::unregisterSelector(Selector & selector, unsigned char flags) {
		_socket->unregisterSelector(selector, flags);
	}

	bool Connection::isReadable(Selector & selector) {
		return _socket->isReadable(selector);
	}

	bool Connection::isWritable(Selector & selector) {
		return _socket->isWritable(selector);
	}

	void Connection::negotiate() {
		_socket->negotiate();
	}

	AutoRef<Socket> Connection::socket() {
		return _socket;
	}

	int Connection::recv(char * buffer, size_t size) {
		int ret = _socket->recv(buffer, size);
		if (ret > 0) {
			_recvLifetime.resetLifetime();
		}
		return ret;
	}

	int Connection::send(const char * data, size_t len) {
		int ret = _socket->send(data, len);
		if (ret > 0) {
			_recvLifetime.resetLifetime();
		}
		return ret;
	}

	void Connection::close() {
		_socket->close();
	}
	bool Connection::isClosed() {
		return _socket->isClosed();
	}

	void Connection::flagTerminate(bool flag) {
		terminateFlag = flag;
	}

	bool Connection::isTerminateFlaged() {
		return terminateFlag;
	}

	bool & Connection::completed() {
		return _completed;
	}

	Packet & Connection::read() {
		_packet.clear();
		int len = recv(_packet.getData(), _packet.getLimit());
		_packet.setPosition(len);
		return _packet;
	}

	Packet & Connection::packet() {
		return _packet;
	}

	InetAddress Connection::getRemoteAddress() {
		return _socket->getRemoteInetAddress();
	}

	InetAddress Connection::getLocalAddress() {
		return _socket->getLocalInetAddress();
	}

	Lifetime & Connection::recvLifetime() {
		return _recvLifetime;
	}

	unsigned long Connection::getRecvTimeout() {
		return _recvTimeout;
	}
	
	void Connection::setRecvTimeout(unsigned long timeout) {
		this->_recvTimeout = timeout;
		_socket->setRecvTimeout(timeout);
	}

	bool Connection::expiredRecvTimeout() {
		return ((_recvTimeout != 0) && (_recvLifetime.lifetime() >= _recvTimeout));
	}
}
