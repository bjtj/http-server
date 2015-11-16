#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

#include <vector>
#include <map>

using namespace std;
using namespace OS;
using namespace UTIL;

static const Logger & logger = LoggerFactory::getDefaultLogger();

// 1. connection manager
//  - add connection
//  - remove connection

// 2. thread way
//  - make thread
//  - make connection session
//  - let it go

// 3. multiplex way
//  - register client fd
//  - bypass read/write states


// Connection
//  - handle recv
//  - handle write
//  - flag Connection closed

class Packet {
private:
	char * buffer;
	size_t size;
	size_t pos;
	size_t limit;

public:
	Packet(size_t size) : buffer(NULL), size(size), limit(size) {
		if (size > 0) {
			buffer = new char[size];
			memset(buffer, 0, size);
		}
	}
	Packet(const Packet & other) {
		size = other.size;
		pos = 0;
		limit = other.limit;
		if (size > 0) {
			buffer = new char[size];
			memcpy(buffer, other.buffer, size);
		} else {
			buffer = NULL;
		}
	}
	virtual ~Packet() {
		if (buffer) {
			delete [] buffer;
		}
	}
	void clear() {
		memset(buffer, 0, size);
		pos = 0;
	}
	size_t remaining() {
		return limit - pos;
	}
	void write(const char * data, size_t len) {
		size_t remain = remaining();
		if (len > remain) {
			throw Exception("overflow", -1, 0);
		}

		memcpy(buffer + pos, data, len);
		pos += len;
	}
	char * getData() {
		return buffer;
	}
	size_t getLength() {
		return pos;
	}
	void setPosition(size_t position) {
		if (position > limit) {
			throw Exception("overlimit", -1, 0);
		}
		this->pos = position;
	}
	size_t getLimit() {
		return limit;
	}
	void resetLimit() {
		limit = size;
	}
	void setLimit(size_t limit) {
		if (limit > size) {
			throw Exception("oversize", -1, 0);
		}
		this->limit = limit;
	}
};

class Connection;

class Communication {
private:
public:
	Communication() {}
	virtual ~Communication() {}
	virtual void onConnected(Connection & connection) = 0;
	virtual void onDataReceived(Connection & connection, Packet & packet) = 0;
	virtual void onWriteeable(Connection & connection) = 0;
	virtual void onDisconnected(Connection & connection) = 0;
	virtual bool isCommunicationCompleted() = 0;
};

class Connection {
private:
	Socket & socket;
	bool terminateSignal;
	bool completed;
	Packet packet;

public:
	Connection(Socket & socket) : socket(socket), terminateSignal(false), completed(false), packet(4096) {
	}
	virtual ~Connection() {
	}

	int getId() {
		return socket.getFd();
	}

	void registerSelector(Selector & selector) {
		socket.registerSelector(selector);
	}

	bool isReadableSelected(Selector & selector) {
		return selector.isReadableSelected(socket);
	}

	bool isWritableSelected(Selector & selector) {
		return selector.isReadableSelected(socket);
	}

	int recv(char * buffer, size_t size) {
		return socket.recv(buffer, size);
	}

	int send(const char * data, size_t len) {
		return socket.send(data, len);
	}

	void close() {
		socket.close();
	}
	bool isClosed() {
		return socket.isClosed();
	}

	void signalTerminate() {
		terminateSignal = true;
	}

	bool isTerminateSignaled() {
		return terminateSignal;
	}

	void setCompleted() {
		completed = true;
	}

	bool isCompleted() {
		return completed;
	}

	void setReadSize(size_t readSize) {
		packet.setLimit(readSize);
	}

	void resetReadSize() {
		packet.resetLimit();
	}

	Packet & read() {
		packet.clear();
		int len = recv(packet.getData(), packet.getLimit());
		packet.setPosition(len);
		return packet;
	}
};

class HttpRequest {
private:
	string header;
public:
	HttpRequest() {}
	virtual ~HttpRequest() {}

	string & getHeader() {
		return header;
	}
};

class HttpCommunication : public Communication {
private:
	HttpRequest request;
	bool requestHeaderDone;
	bool requestHeaderHandled;
	string response;
	bool writeable;
	bool responseHeaderTransferDone;
	bool responseContentTransferDone;
	bool communicationCompleted;

public:
	HttpCommunication() : requestHeaderDone(false), requestHeaderHandled(false), writeable(false), responseHeaderTransferDone(false), responseContentTransferDone(false), communicationCompleted(false) {
	}
	virtual ~HttpCommunication() {
	}

	virtual void onConnected(Connection & connection) {

		connection.setReadSize(1);
	}

	virtual void onDataReceived(Connection & connection, Packet & packet) {

		readHeader(connection, packet);
		if (requestHeaderDone && !requestHeaderHandled) {
			onRequest(request);
		}
	}

	void readHeader(Connection & connection, Packet & packet) {

		if (!requestHeaderDone) {
			string chunk(packet.getData(), packet.getLength());
			request.getHeader().append(chunk);
			packet.clear();

			if (Text::endsWith(request.getHeader(), "\r\n\r\n")) {

				logger.logv(request.getHeader());

				requestHeaderDone = true;
				connection.resetReadSize();
			}
		}
	}

	void onRequest(HttpRequest & request) {
		response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello";
		writeable = true;
	}

	virtual void onWriteeable(Connection & connection) {

		if (!writeable) {
			return;
		}

		if (!responseHeaderTransferDone) {
			connection.send(response.c_str(), response.length());
			responseHeaderTransferDone = true;
			responseContentTransferDone = true;
		}

		if (!responseContentTransferDone) {
			// send content if needed
		}

		if (responseContentTransferDone) {
			communicationCompleted = true;
		}
	}

	

	virtual void onDisconnected(Connection & connection) {
		logger.logv("onDisconnected");
	}

	virtual bool isCommunicationCompleted() {
		return communicationCompleted;
	}
};

class ConnectionThread : public Thread {
private:
	Connection & connection;
	Communication & communication;
	Selector selector;

public:
	ConnectionThread(Connection & connection, Communication & communication) : connection(connection), communication(communication) {
	}
	virtual ~ConnectionThread() {
	}

	virtual void run() {

		connection.registerSelector(selector);

		try {

			communication.onConnected(connection);

			while (!interrupted() && !connection.isTerminateSignaled()) {

				if (selector.select(1000) > 0) {

					if (connection.isReadableSelected(selector)) {
						Packet & packet = connection.read();
						communication.onDataReceived(connection, packet);
					}

					if (connection.isWritableSelected(selector)) {
						communication.onWriteeable(connection);
					}
				}

				if (connection.isClosed() || communication.isCommunicationCompleted()) {
					break;
				}
			}

		} catch (IOException e) {
			logger.loge(e.getMessage());
		}

		// notify disconnection to connection manager
		communication.onDisconnected(connection);
		connection.setCompleted();

		delete &communication;
	}
};

class ConnectionManager {
private:
	ServerSocket * serverSocket;
	Selector selector;
	map<int, Connection*> connectionTable;
	vector<Thread*> threads;
	Semaphore connectionsLock;

public:
	ConnectionManager() : serverSocket(NULL), connectionsLock(1) {
	}
	virtual ~ConnectionManager() {
		stop();
	}

	virtual Connection * makeConnection(Socket & client) {
		return new Connection(client);
	}

	virtual void removeConnection(Connection * connection) {
		delete connection;
	}

	void onConnect(Socket & client) {
		Connection * connection = makeConnection(client);
		Communication * communication = new HttpCommunication;

		connectionsLock.wait();
		connectionTable[connection->getId()] = connection;
		connectionsLock.post();

		ConnectionThread * thread = new ConnectionThread(*connection, *communication);
		threads.push_back(thread);
		thread->start();
	}

	void onDisconnect(Connection * connection) {

		connectionsLock.wait();
		int id = connection->getId();
		if (connectionTable.find(id) != connectionTable.end()) {
			removeConnection(connection);
			connectionTable.erase(id);
		}
		
		connectionsLock.post();
	}

	void clearConnections() {

		connectionsLock.wait();
		for (map<int, Connection*>::const_iterator iter = connectionTable.begin(); iter != connectionTable.end(); iter++) {
			Connection * connection = iter->second;

			connection->signalTerminate();
			while (!connection->isCompleted()) {
				idle(10);
			}

			removeConnection(connection);
		}
		connectionTable.clear();
		connectionsLock.post();
	}

	void removeCompletedConnections() {

		connectionsLock.wait();
		for (map<int, Connection*>::const_iterator iter = connectionTable.begin(); iter != connectionTable.end();) {
			Connection * connection = iter->second;
			if (connection->isCompleted()) {
				removeConnection(connection);
				iter = connectionTable.erase(iter);
			} else {
				iter++;
			}
		}
		connectionsLock.post();
	}

	void start(int port) {
		if (serverSocket) {
			return;
		}

		serverSocket = new ServerSocket(port);
		serverSocket->setReuseAddr();
		serverSocket->bind();
		serverSocket->listen(5);

		serverSocket->registerSelector(selector);
	}

	void poll(unsigned long timeout) {

		if (selector.select(timeout) > 0) {
			if (selector.isReadableSelected(*serverSocket)) {
				Socket * client = serverSocket->accept();
				if (client) {
					onConnect(*client);
				}
			}
		}

		removeCompletedConnections();
		removeCompletedThreads();
	}

	void removeCompletedThreads() {
		for (vector<Thread*>::const_iterator iter = threads.begin(); iter != threads.end();) {

			Thread * thread = *iter;
			if (!thread->isRunning()) {
				delete thread;
				iter = threads.erase(iter);
			} else {
				iter++;
			}
		}
	}

	void stop() {

		if (!serverSocket) {
			return;
		}

		clearConnections();

		stopAllThreads();

		serverSocket->unregisterSelector(selector);
		serverSocket->close();

		serverSocket = NULL;
	}

	void stopAllThreads() {
		for (size_t i = 0; i < threads.size(); i++) {
			Thread * thread = threads[i];
			thread->interrupt();
			thread->join();
			delete thread;
		}
		threads.clear();
	}
};

int main(int argc, char * args[]) {

	bool done = false;
	ConnectionManager cm;

	cm.start(8083);

	while (!done) {
		cm.poll(1000);
	}

	cm.stop();

	return 0;
}