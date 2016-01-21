#include "Connection.hpp"

namespace HTTP {
    
    using namespace std;
    using namespace OS;

    Connection::Connection(Socket & socket) : socket(socket), terminateSignal(false), completed(false), packet(4096) {
    }
    Connection::~Connection() {
    }
    
    int Connection::getId() {
        return socket.getFd();
    }
    
    bool Connection::isSelectable() {
        return socket.isSelectable();
    }
    
    void Connection::registerSelector(Selector & selector) {
        socket.registerSelector(selector);
    }

	void Connection::unregisterSelector(Selector & selector) {
        socket.unregisterSelector(selector);
    }
    
    bool Connection::isReadableSelected(Selector & selector) {
        return selector.isReadableSelected(socket);
    }
    
    bool Connection::isWritableSelected(Selector & selector) {
        return selector.isWriteableSelected(socket);
    }
    
    int Connection::recv(char * buffer, size_t size) {
        return socket.recv(buffer, size);
    }
    
    int Connection::send(const char * data, size_t len) {
        return socket.send(data, len);
    }
    
    void Connection::close() {
        socket.close();
    }
    bool Connection::isClosed() {
        return socket.isClosed();
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
        return socket.getRemoteInetAddress();
    }

	InetAddress Connection::getLocalAddress() {
        return socket.getLocalInetAddress();
    }
}
