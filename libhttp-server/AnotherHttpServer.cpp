#include "AnotherHttpServer.hpp"
#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include "StringDataSource.hpp"
#include "FileDataSource.hpp"
#include "StringDataSink.hpp"
#include <liboslayer/FileReaderWriter.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getLogger(__FILE__);

	/**
	 * @brief HttpRequestHandler
	 */

	HttpRequestHandler::HttpRequestHandler() {
	}
	HttpRequestHandler::~HttpRequestHandler() {
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
    
	void HttpRequestHandler::setFixedTransfer(HttpResponse & response, const string & content) {

		AutoRef<DataSource> source(new StringDataSource(content));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, content.size()));

        response.clearTransfer();
        response.setTransfer(transfer);
		response.setContentLength(content.size());
	}

	void HttpRequestHandler::setFileTransfer(HttpResponse & response, const string & filepath) {

		File file(filepath);
		setFileTransfer(response, file);
	}

	void HttpRequestHandler::setFileTransfer(HttpResponse & response, OS::File & file) {

		AutoRef<DataSource> source(new FileDataSource(FileStream(file, "rb")));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, file.getSize()));

		response.clearTransfer();
		response.setTransfer(transfer);
		response.setContentLength((unsigned long long)file.getSize());
	}

	void HttpRequestHandler::setPartialFileTransfer(HttpResponse & response, OS::File & file, size_t start, size_t end) {

		if (file.getSize() < 1) {
			throw Exception("empty file");
		}

		if (end == 0 || end >= (size_t)file.getSize()) {
			end = (size_t)file.getSize() - 1;
		}

		if (start >= end) {
			throw Exception("wrong range start error");
		}

		size_t size = (end - start + 1);

		FileStream stream(file, "rb");
		stream.seek(start);
		AutoRef<DataSource> source(new FileDataSource(stream));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, size));

		response.setStatusCode(206);
		response.setContentLength(size);
		string bytes = "bytes=";
		bytes.append(Text::toString(start));
		bytes.append("-");
		bytes.append(Text::toString(end));
		bytes.append("/");
		bytes.append(Text::toString(file.getSize()));
		response.getHeader().setHeaderField("Content-Range", bytes);
		response.setTransfer(transfer);
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

	HttpCommunication::HttpCommunication(AutoRef<HttpRequestHandlerDispatcher> dispatcher) : requestHeaderHandled(false), writeable(false), responseHeaderTransferDone(false), responseContentTransferDone(false), communicationCompleted(false), dispatcher(dispatcher) {
	}

	HttpCommunication::~HttpCommunication() {
	}

	void HttpCommunication::onConnected(Connection & connection) {
        request.setRemoteAddress(connection.getRemoteAddress());
		request.setLocalAddress(connection.getLocalAddress());
	}

	void HttpCommunication::onReceivable(Connection & connection) {

		if (writeable) {
			return;
		}

		readRequestHeaderIfNeed(connection);
		if (requestHeaderReader.complete()) {
            
            if (!requestHeaderHandled) {
                
                onRequestHeader(request, response);
                requestContentReadCounter.setContentSize(request.getContentLength());
                
            } else {
                
                AutoRef<DataTransfer> transfer = request.getTransfer();
				if (!transfer.nil()) {
					transfer->recv(connection);
					readRequestContent(request, response, connection.getPacket());
					if (transfer->completed()) {
						AutoRef<DataSink> sink = transfer->sink();
						onHttpRequestContentCompleted(request, sink, response);
						writeable = true;
					}
				} else {
					onHttpRequestContentCompleted(request, AutoRef<DataSink>(), response);
                    writeable = true;
				}
            }
		}
	}

	void HttpCommunication::readRequestHeaderIfNeed(Connection & connection) {

		if (!requestHeaderReader.complete()) {
           
			connection.setReadSize(1);
			Packet & packet = connection.read();
            requestHeaderReader.read(packet.getData(), (int)packet.getLength());

			packet.clear();

			if (requestHeaderReader.complete()) {
                request.setHeader(requestHeaderReader.getHeader());
				connection.resetReadLimit();
			}
		}
	}
    
    void HttpCommunication::readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
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
			handler->onHttpRequestHeaderCompleted(request, response);
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
            handler->onHttpRequestContentCompleted(request, sink, response);
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

	void HttpCommunication::onWriteable(Connection & connection) {

		if (!writeable) {
			return;
		}

		if (!responseHeaderTransferDone) {
            sendResponseHeader(connection);
		}

		if (!responseContentTransferDone) {
			sendResponseContent(connection);
		}

		if (responseContentTransferDone) {
			communicationCompleted = true;
			AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
			if (!handler.nil()) {
				handler->onHttpResponseTransferCompleted(request, response);
			}
		}
	}

	void HttpCommunication::sendResponseHeader(Connection & connection) {

		HttpResponseHeader & header = response.getHeader();
            
        string headerString = header.toString();
        connection.send(headerString.c_str(), (int)headerString.length());
		// TODO: check write length and compare the header string length
		responseHeaderTransferDone = true;

		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
		if (!handler.nil()) {
			handler->onHttpResponseHeaderCompleted(request, response);
		}
            
        if (!header.isChunkedTransfer() && header.getContentLength() == 0) {
            responseContentTransferDone = true;
        }
	}

	void HttpCommunication::sendResponseContent(Connection & connection) {

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

	void HttpCommunication::onDisconnected(Connection & connection) {
	}

	bool HttpCommunication::isCommunicationCompleted() {
		return communicationCompleted;
	}

	void HttpCommunication::handleError(HttpRequest & request, HttpResponse & response, int errorCode) {

		response.setStatusCode(errorCode);
		response.getHeader().setConnection("close");

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

			logger->loge("ConnectinoManager / status : " + cm.getStatus());
			
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
		port(config.getIntegerProperty("listen.port")),
		dispatcher(AutoRef<HttpRequestHandlerDispatcher>(new SimpleHttpRequestHandlerDispatcher)),
		connectionManager(AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)), config.getIntegerProperty("thread.count", 20)),
		thread(NULL) {

		connectionManager.setOnMaxCapacity(AutoRef<OnMaxCapacity>(new MaxClientHandler));
	}
    
    AnotherHttpServer::AnotherHttpServer(HttpServerConfig config, AutoRef<ServerSocketMaker> serverSocketMaker) :
		port(config.getIntegerProperty("listen.port")),
		dispatcher(AutoRef<HttpRequestHandlerDispatcher>(new SimpleHttpRequestHandlerDispatcher)),
		connectionManager(AutoRef<CommunicationMaker>(new HttpCommunicationMaker(dispatcher)), config.getIntegerProperty("thread.count", 20), serverSocketMaker),
		thread(NULL) {

		connectionManager.setOnMaxCapacity(AutoRef<OnMaxCapacity>(new MaxClientHandler));
    }
    
	AnotherHttpServer::~AnotherHttpServer() {
	}

	void AnotherHttpServer::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		dispatcher->registerRequestHandler(pattern, handler);
	}
    
	void AnotherHttpServer::unregisterRequestHandler(const string & pattern) {
		dispatcher->unregisterRequestHandler(pattern);
	}

	int AnotherHttpServer::getPort() {
		return port;
	}

	void AnotherHttpServer::start() {
		connectionManager.start(port);
	}

	void AnotherHttpServer::startAsync() {
		start();

		if (!thread) {
			thread = new ServerPollingThread(*this);
			thread->start();
		}
	}

	void AnotherHttpServer::poll(unsigned long timeout) {
		connectionManager.poll(timeout);
	}

	void AnotherHttpServer::stop() {

		if (thread) {
			thread->interrupt();
			thread->join();
			delete thread;
			thread = NULL;
		}

		connectionManager.stop();
	}

}
