#include "Connection.hpp"

namespace HTTP {
    
    using namespace std;
    using namespace OS;
	using namespace UTIL;

    Connection::Connection(AutoRef<Socket> socket) : socket(socket), terminateSignal(false), completed(false), packet(4096), id(socket->getFd()) {
    }
    Connection::~Connection() {
    }
    
    int Connection::getId() {
		return id;
    }
    
    bool Connection::isSelectable() {
        return socket->isSelectable();
    }
    
    void Connection::registerSelector(Selector & selector, unsigned long flags) {
        socket->registerSelector(selector, flags);
    }

	void Connection::unregisterSelector(Selector & selector, unsigned long flags) {
        socket->unregisterSelector(selector, flags);
    }
    
    bool Connection::isReadableSelected(Selector & selector) {
        return selector.isReadableSelected(*socket);
    }
    
    bool Connection::isWritableSelected(Selector & selector) {
        return selector.isWritableSelected(*socket);
    }

	void Connection::negotiate() {
		socket->negotiate();
	}
    
    int Connection::recv(char * buffer, size_t size) {
		int ret = socket->recv(buffer, size);
        return ret;
    }
    
    int Connection::send(const char * data, size_t len) {
        return socket->send(data, len);
    }
    
    void Connection::close() {
        socket->close();
    }
    bool Connection::isClosed() {
        return socket->isClosed();
    }
    
    void Connection::signalTerminate() {
        terminateSignal = true;
    }
    
    bool Connection::isTerminateSignaled() {
        return terminateSignal;
    }
    
    void Connection::setCompleted() {
        completed = true;
    }
    
    bool Connection::isCompleted() {
        return completed;
    }
    
    void Connection::setReadSize(size_t readSize) {
        packet.setLimit(readSize);
    }
    
	size_t Connection::getLimit() {
		return packet.getLimit();
	}

    void Connection::resetReadLimit() {
        packet.resetLimit();
    }
    
    Packet & Connection::read() {
        packet.clear();
        int len = recv(packet.getData(), packet.getLimit());
        packet.setPosition(len);
        return packet;
    }

	Packet & Connection::getPacket() {
		return packet;
	}
    
    InetAddress Connection::getRemoteAddress() {
        return socket->getRemoteInetAddress();
    }

	InetAddress Connection::getLocalAddress() {
        return socket->getLocalInetAddress();
    }
}
