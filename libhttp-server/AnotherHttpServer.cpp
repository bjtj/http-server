#include "AnotherHttpServer.hpp"
#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include "StringDataSource.hpp"
#include "FileDataSource.hpp"
#include "StringDataSink.hpp"
#include <liboslayer/Logger.hpp>
#include <liboslayer/File.hpp>

namespace http {

	using namespace std;
	using namespace osl;

	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));

	/**
	 * @brief AnotherHttpServer
	 */

	class ServerPollingThread : public Thread {
	private:
		AnotherHttpServer & server;
	public:
		ServerPollingThread(AnotherHttpServer & server) : server(server) {
		}
		virtual ~ServerPollingThread() {
		}

		virtual void run() {
			while (!interrupted()) {
				server.poll(100);
			}
		}
	};

	/**
	 *
	 */
	class MaxClientHandler : public OnMaxCapacity {
	public:
		MaxClientHandler() {}
		virtual ~MaxClientHandler() {}
		virtual void onMaxCapacity(ConnectionManager & cm, AutoRef<Connection> connection) {

			logger->error("ConnectinoManager / free : " + Text::toString(cm.available()) +
						  ", work : " + Text::toString(cm.working()));
			
			string content = "<html><head><title>Not available</title></head>"
				"<body><h1>Too many connections</h1><p>Try again</p></body></html>";
			string header = "HTTP/1.1 503 Service Not Available\r\nContent-Length: " +
				Text::toString(content.size()) + "\r\nContent-Type: text/html\r\n\r\n";
			connection->send(header.c_str(), header.size());
			connection->send(content.c_str(), content.size());
		}
	};

	/**
	 *
	 */
	AnotherHttpServer::AnotherHttpServer(HttpServerConfig config) :
		config(config),
		dispatcher(AutoRef<HttpRequestHandlerDispatcher>(new SimpleHttpRequestHandlerDispatcher)),
		_connectionManager(
			ConnectionConfig(
				AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)),
				config.getIntegerProperty("thread.count", 20))),
		thread(NULL)
	{
		_connectionManager.setOnMaxCapacity(AutoRef<OnMaxCapacity>(new MaxClientHandler));
		_connectionManager.setRecvTimeout(config.getIntegerProperty("recv.timeout", 0));
	}
    
    AnotherHttpServer::AnotherHttpServer(HttpServerConfig config, AutoRef<ServerSocketMaker> serverSocketMaker) :
		config(config),
		dispatcher(AutoRef<HttpRequestHandlerDispatcher>(new SimpleHttpRequestHandlerDispatcher)),
		_connectionManager(
			ConnectionConfig(
				AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)),
				serverSocketMaker,
				config.getIntegerProperty("thread.count", 20))),
		thread(NULL)
	{
		_connectionManager.setOnMaxCapacity(AutoRef<OnMaxCapacity>(new MaxClientHandler));
		_connectionManager.setRecvTimeout(config.getIntegerProperty("recv.timeout", 0));
    }
    
	AnotherHttpServer::~AnotherHttpServer() {
		/**/
	}

	void AnotherHttpServer::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		dispatcher->registerRequestHandler(pattern, handler);
	}
    
	void AnotherHttpServer::unregisterRequestHandler(const string & pattern) {
		dispatcher->unregisterRequestHandler(pattern);
	}

	InetAddress AnotherHttpServer::getServerAddress() {
		return _connectionManager.getServerAddress();
	}

	void AnotherHttpServer::start() {
		_connectionManager.start(config.getIntegerProperty("listen.port", 80),
								 config.getIntegerProperty("backlog", 5));
	}

	void AnotherHttpServer::startAsync() {
		start();
		if (!thread) {
			thread = new ServerPollingThread(*this);
			thread->start();
		}
	}

	void AnotherHttpServer::poll(unsigned long timeout) {
		_connectionManager.poll(timeout);
	}

	void AnotherHttpServer::stop() {
		if (thread) {
			thread->interrupt();
			thread->wait();
			delete thread;
			thread = NULL;
		}
		_connectionManager.stop();
	}

	size_t AnotherHttpServer::connections() {
		return _connectionManager.working();
	}
    
    ConnectionManager & AnotherHttpServer::connectionManager() {
        return _connectionManager;
    }
}
