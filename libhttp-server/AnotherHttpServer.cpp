#include "AnotherHttpServer.hpp"
#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include "StringDataSource.hpp"
#include "FileDataSource.hpp"
#include "StringDataSink.hpp"
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

	/**
	 * @brief HttpRequestHandler
	 */

	HttpRequestHandler::HttpRequestHandler() {
	}
	HttpRequestHandler::~HttpRequestHandler() {
	}

	AutoRef<DataSink> HttpRequestHandler::getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	void HttpRequestHandler::onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		// request header transfer done
	}
	void HttpRequestHandler::onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		// request content transfer done
	}
	void HttpRequestHandler::onHttpResponseHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		// response header transfer done
	}
	void HttpRequestHandler::onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response) {
		// response content transfer done
	}
	bool HttpRequestHandler::onException(HttpRequest & request, HttpResponse & response, Exception & ex) {
		return false;
	}
	
	/**
	 * @brief SimpleHttpRequestHandlerDispatcher
	 */

	SimpleHttpRequestHandlerDispatcher::SimpleHttpRequestHandlerDispatcher() {
	}

	SimpleHttpRequestHandlerDispatcher::~SimpleHttpRequestHandlerDispatcher() {
	}

	void SimpleHttpRequestHandlerDispatcher::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		handlers.push_back(RequestHandlerNode(pattern, handler));
	}

	void SimpleHttpRequestHandlerDispatcher::unregisterRequestHandler(const string & pattern) {
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end();) {
			RequestHandlerNode & node = *iter;
			if (node.equalsPattern(pattern)) {
				iter = handlers.erase(iter);
			} else {
				iter++;
			}
		}
	}

	AutoRef<HttpRequestHandler> SimpleHttpRequestHandlerDispatcher::getRequestHandler(const string & path) {
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end(); iter++) {
			RequestHandlerNode & node = *iter;
			if (node.patternMatch(path)) {
				return node.getHandler();
			}
		}
		return AutoRef<HttpRequestHandler>();
	}
	
	/**
	 * @brief HttpCommunication
	 */

	HttpCommunication::HttpCommunication(AutoRef<HttpRequestHandlerDispatcher> dispatcher)
		: requestHeaderHandled(false),
		  writeable(false),
		  responseHeaderTransferDone(false),
		  responseContentTransferDone(false),
		  communicationCompleted(false),
		  dispatcher(dispatcher) {
	}

	HttpCommunication::~HttpCommunication() {
	}

	void HttpCommunication::reset() {
		request.clear();
		requestHeaderReader.clear();
		response.clear();
		requestHeaderHandled = false;
		writeable = false;
		responseHeaderTransferDone = false;
		responseContentTransferDone = false;
		communicationCompleted = false;
	}

	void HttpCommunication::onConnected(AutoRef<Connection> connection) {
        request.setRemoteAddress(connection->getRemoteAddress());
		request.setLocalAddress(connection->getLocalAddress());
	}

	bool HttpCommunication::isReadable() {
		return writeable == false;
	}
	
	bool HttpCommunication::isWritable() {
		return writeable == true;
	}

	bool HttpCommunication::onReceivable(UTIL::AutoRef<Connection> connection) {
		if (writeable) {
			return false;
		}
		readRequestHeaderIfNeed(connection);
		if (!requestHeaderReader.complete()) {
            return true;
        }
        if (!requestHeaderHandled) {
            onRequestHeader(request, response);
            return true;
        }
        AutoRef<DataTransfer> transfer = request.getTransfer();
        if (!transfer.nil()) {
            transfer->recv(connection);
            readRequestContent(request, response, connection->packet());
            if (transfer->completed()) {
                AutoRef<DataSink> sink = transfer->sink();
                onHttpRequestContentCompleted(request, sink, response);
                writeable = true;
            }
        } else {
            onHttpRequestContentCompleted(request, AutoRef<DataSink>(), response);
            writeable = true;
        }
        return true;
	}

	void HttpCommunication::readRequestHeaderIfNeed(UTIL::AutoRef<Connection> connection) {
		if (!requestHeaderReader.complete()) {
			connection->packet().setLimit(1);
			Packet & packet = connection->read();
            requestHeaderReader.read(packet.getData(), (int)packet.getLength());
			packet.clear();
			if (requestHeaderReader.complete()) {
                request.setHeader(requestHeaderReader.getHeader());
				connection->packet().restoreLimit();
			}
		}
	}
    
    void HttpCommunication::readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
		//
    }

	void HttpCommunication::onRequestHeader(HttpRequest & request, HttpResponse & response) {
		AutoRef<DataSink> sink;
		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
		if (!handler.nil()) {
			sink = handler->getDataSink();
		}
		if (sink.nil()) {
			sink = AutoRef<DataSink>(new StringDataSink);
		}
        prepareRequestContentTransfer(request, sink);
		if (!handler.nil()) {
			try {
				handler->onHttpRequestHeaderCompleted(request, response);
			} catch (Exception e) {
				if (!handler->onException(request, response, e)) {
					handleError(request, response, 500);
				}
			}
		}
        AutoRef<DataTransfer> transfer = request.getTransfer();
        if (transfer.empty()) {
            onHttpRequestContentCompleted(request, AutoRef<DataSink>(), response);
			writeable = true;
		}
        requestHeaderHandled = true;
	}

	void HttpCommunication::onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
        if (!handler.nil()) {
			try {
				handler->onHttpRequestContentCompleted(request, sink, response);
			} catch (Exception e) {
				if (!handler->onException(request, response, e)) {
					handleError(request, response, 500);
				}
			}
        } else {
			handleError(request, response, 404);
		}
	}

	void HttpCommunication::prepareRequestContentTransfer(HttpRequest & request, AutoRef<DataSink> sink) {
		if (request.getHeader().isChunkedTransfer()) {
			request.setTransfer(AutoRef<DataTransfer>(new ChunkedTransfer(sink)));
        } else {
            if (request.getContentLength() > 0) {
                AutoRef<DataTransfer> transfer(new FixedTransfer(sink, request.getContentLength()));
				request.setTransfer(transfer);
            }
        }
	}

	bool HttpCommunication::onWriteable(UTIL::AutoRef<Connection> connection) {
		if (!writeable) {
			return false;
		}
		if (!responseHeaderTransferDone) {
            sendResponseHeader(connection);
		}
		if (!responseContentTransferDone) {
			sendResponseContent(connection);
		}
		if (responseContentTransferDone) {
			AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
			if (!handler.nil()) {
				handler->onHttpResponseTransferCompleted(request, response);
			}
			communicationCompleted = true;
			if (request.getHeader().getProtocol() == "HTTP/1.1" &&
				Text::equalsIgnoreCase(request.getHeaderFieldIgnoreCase("Connection"), "close") == false &&
				Text::equalsIgnoreCase(response.getHeaderFieldIgnoreCase("Connection"), "close") == false) {
				reset();
			}
		}
		return true;
	}

	void HttpCommunication::sendResponseHeader(UTIL::AutoRef<Connection> connection) {
		HttpResponseHeader & header = response.header();
        string headerString = header.toString();
        if (connection->send(headerString.c_str(), (int)headerString.length()) != (int)headerString.length()) {
            // TODO: retry
            throw Exception("Response Header send failed");
        }
		responseHeaderTransferDone = true;
		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
		if (!handler.nil()) {
			handler->onHttpResponseHeaderCompleted(request, response);
		}
        if (!header.isChunkedTransfer() && header.getContentLength() == 0) {
            responseContentTransferDone = true;
        }
	}

	void HttpCommunication::sendResponseContent(UTIL::AutoRef<Connection> connection) {
        AutoRef<DataTransfer> responseTransfer = response.getTransfer();
		if (!responseTransfer.empty()) {
			responseTransfer->send(connection);
			if (responseTransfer->completed()) {
				responseContentTransferDone = true;
			}
		} else {
			responseContentTransferDone = true;
		}
	}

	void HttpCommunication::onDisconnected(UTIL::AutoRef<Connection> connection) {
	}

	bool HttpCommunication::isCommunicationCompleted() {
		return communicationCompleted;
	}

	void HttpCommunication::handleError(HttpRequest & request, HttpResponse & response, int errorCode) {

		response.setStatus(errorCode);

		switch (errorCode) {
		case 404:
			response.setContentType("text/html");
			string content = "<html><head><title>404 not found</title></head><body><h1>404 not found</h1><p>Sorry, page not found.</p></body></html>";
			response.setContentLength(content.length());
			response.setTransfer(AutoRef<DataTransfer>(new FixedTransfer(AutoRef<DataSource>(new StringDataSource(content)), content.size())));
			break;
		}
	}
	
	/**
	 * @brief HttpCommunicationMaker
	 */
	
    HttpCommunicationMaker::HttpCommunicationMaker(AutoRef<HttpRequestHandlerDispatcher> dispatcher) : dispatcher(dispatcher) {
    }

    HttpCommunicationMaker::~HttpCommunicationMaker() {
    }
    
    AutoRef<Communication> HttpCommunicationMaker::makeCommunication() {
        return AutoRef<Communication>(new HttpCommunication(dispatcher));
    }


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

			logger->loge("ConnectinoManager / free : " + Text::toString(cm.available()) +
						 ", work : " + Text::toString(cm.working()));
			
			string content = "<html><head><title>Not available</title></head><body><h1>Too much connection</h1><p>Try again</p></body></html>";
			string header = "HTTP/1.1 503 Service Not Available\r\nContent-Length: " + Text::toString(content.size()) + "\r\nContent-Type: text/html\r\n\r\n";
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
		_connectionManager(AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)),
						  config.getIntegerProperty("thread.count", 20)),
		thread(NULL)
	{

		_connectionManager.setOnMaxCapacity(AutoRef<OnMaxCapacity>(new MaxClientHandler));
		_connectionManager.setRecvTimeout(config.getIntegerProperty("recv.timeout", 0));
	}
    
    AnotherHttpServer::AnotherHttpServer(HttpServerConfig config, AutoRef<ServerSocketMaker> serverSocketMaker) :
		config(config),
		dispatcher(AutoRef<HttpRequestHandlerDispatcher>(new SimpleHttpRequestHandlerDispatcher)),
		_connectionManager(AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)),
						  config.getIntegerProperty("thread.count", 20), serverSocketMaker),
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

	int AnotherHttpServer::getPort() {
		return config.getIntegerProperty("listen.port", 80);
	}

	void AnotherHttpServer::start() {
		_connectionManager.start(getPort(), config.getIntegerProperty("backlog", 5));
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
