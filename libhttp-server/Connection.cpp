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

	bool Connection::isSelectable() {
		return _socket->isSelectable();
	}

	void Connection::registerSelector(Selector & selector, unsigned char flags) {
		_socket->registerSelector(selector, flags);
	}

	void Connection::unregisterSelector(Selector & selector, unsigned char flags) {
		_socket->unregisterSelector(selector, flags);
	}

	bool Connection::isReadableSelected(Selector & selector) {
		return selector.isReadableSelected(*_socket);
	}

	bool Connection::isWritableSelected(Selector & selector) {
		return selector.isWritableSelected(*_socket);
	}

	void Connection::negotiate() {
		_socket->negotiate();
	}

	AutoRef<Socket> Connection::socket() {
		return _socket;
	}

	int Connection::recv(char * buffer, size_t size) {
		_recvLifetime.resetLifetime();
		int ret = _socket->recv(buffer, size);
		return ret;
	}

	int Connection::send(const char * data, size_t len) {
		return _socket->send(data, len);
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

	unsigned long & Connection::recvTimeout() {
		return _recvTimeout;
	}

	bool Connection::expiredRecvTimeout() {
		return ((_recvTimeout != 0) && (_recvLifetime.lifetime() >= _recvTimeout));
	}
}
