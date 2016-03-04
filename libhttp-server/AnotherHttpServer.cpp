#include "AnotherHttpServer.hpp"

#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include "FileTransfer.hpp"

#include <liboslayer/FileReaderWriter.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief HttpRequestHandler
	 */

	HttpRequestHandler::HttpRequestHandler() {
	}
	HttpRequestHandler::~HttpRequestHandler() {
	}

	void HttpRequestHandler::onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
	}
	void HttpRequestHandler::onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
	}
	void HttpRequestHandler::onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
	}
    
	void HttpRequestHandler::setFixedTransfer(HttpResponse & response, const string & content) {

		AutoRef<DataTransfer> transfer(new FixedTransfer(content.length()));
        ChunkedBuffer & cb = ((FixedTransfer*)&transfer)->getChunkedBuffer();
        cb.write(content.c_str(), content.length());
        cb.resetPosition();

        response.clearTransfer();
        response.setTransfer(transfer);
		response.setContentLength(content.length());
	}

	void HttpRequestHandler::setFileTransfer(HttpResponse & response, const string & filepath) {

		File file(filepath);
		setFileTransfer(response, file);
	}

	void HttpRequestHandler::setFileTransfer(HttpResponse & response, OS::File & file) {

		AutoRef<DataTransfer> transfer(new FileTransfer(new FileReader(file), file.getSize()));

		response.clearTransfer();
		response.setTransfer(transfer);
		response.setContentLength((unsigned long long)file.getSize());
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
		return NULL;
	}
	
	/**
	 * @brief HttpCommunication
	 */

	HttpCommunication::HttpCommunication(HttpRequestHandlerDispatcher * dispatcher) : requestHeaderHandled(false), writeable(false), responseHeaderTransferDone(false), responseContentTransferDone(false), communicationCompleted(false), dispatcher(dispatcher) {
	}

	HttpCommunication::~HttpCommunication() {
	}

	void HttpCommunication::onConnected(Connection & connection) {
        request.setRemoteAddress(connection.getRemoteAddress());
		request.setLocalAddress(connection.getLocalAddress());
	}

	void HttpCommunication::onReceivable(Connection & connection) {

		readRequestHeaderIfNeed(connection);
		if (requestHeaderReader.complete()) {
            
            if (!requestHeaderHandled) {
                
                onRequestHeader(request, response);
                requestContentReadCounter.setContentSize(request.getContentLength());
                
            } else {
                
                AutoRef<DataTransfer> transfer = request.getTransfer();
				if (!transfer.empty()) {
					transfer->recv(connection);
					readRequestContent(request, response, connection.getPacket());
					if (transfer->isCompleted()) {
						onHttpRequestContentCompleted(request, response);
						writeable = true;
					}
				} else {
					onHttpRequestContentCompleted(request, response);
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
		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
		if (!handler.nil()) {
			handler->onHttpRequestContent(request, response, packet);
		}
    }

	void HttpCommunication::onRequestHeader(HttpRequest & request, HttpResponse & response) {
        
        prepareRequestContentTransfer(request);
        
		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
		if (!handler.nil()) {
			handler->onHttpRequestHeaderCompleted(request, response);
		}

        AutoRef<DataTransfer> transfer = request.getTransfer();
        if (transfer.empty()) {
            onHttpRequestContentCompleted(request, response);
			writeable = true;
		}
        
        requestHeaderHandled = true;
	}

	void HttpCommunication::onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {

		AutoRef<HttpRequestHandler> handler = dispatcher->getRequestHandler(request.getPath());
        if (!handler.nil()) {
            handler->onHttpRequestContentCompleted(request, response);
        } else {
			handleError(request, response, 404);
		}
	}

	void HttpCommunication::prepareRequestContentTransfer(HttpRequest & request) {

		if (request.getHeader().isChunkedTransfer()) {
			request.setTransfer(new ChunkedTransfer);
        } else {
            if (request.getContentLength() > 0) {
                FixedTransfer * transfer = new FixedTransfer(request.getContentLength());
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
		}
	}

	void HttpCommunication::sendResponseHeader(Connection & connection) {

		HttpResponseHeader & header = response.getHeader();
            
        string headerString = header.toString();
        connection.send(headerString.c_str(), (int)headerString.length());
		// TODO: check write length and compare the header string length
		responseHeaderTransferDone = true;
            
        if (!header.isChunkedTransfer() && header.getContentLength() == 0) {
            responseContentTransferDone = true;
        }
	}

	void HttpCommunication::sendResponseContent(Connection & connection) {

        AutoRef<DataTransfer> responseTransfer = response.getTransfer();

		if (!responseTransfer.empty()) {
			responseTransfer->send(connection);
			if (responseTransfer->isCompleted()) {
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
			response.setTransfer(new FixedTransfer(content));
			break;
		}
	}
	
	/**
	 * @brief HttpCommunicationMaker
	 */
	
    HttpCommunicationMaker::HttpCommunicationMaker(HttpRequestHandlerDispatcher * dispatcher) : dispatcher(dispatcher) {
    }

    HttpCommunicationMaker::~HttpCommunicationMaker() {
    }
    
    Communication * HttpCommunicationMaker::makeCommunication() {
        return new HttpCommunication(dispatcher);
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

	AnotherHttpServer::AnotherHttpServer(HttpServerConfig config) : port(config.getIntegerProperty("listen.port")), httpCommunicationMaker(&dispatcher), connectionManager(httpCommunicationMaker, config.getIntegerProperty("thread.count", 20)), thread(NULL) {
	}
    
    AnotherHttpServer::AnotherHttpServer(HttpServerConfig config, ServerSocketMaker * serverSocketMaker) : port(config.getIntegerProperty("listen.port")), httpCommunicationMaker(&dispatcher), connectionManager(httpCommunicationMaker, config.getIntegerProperty("thread.count", 20), serverSocketMaker), thread(NULL) {
    }
    
	AnotherHttpServer::~AnotherHttpServer() {
	}

	void AnotherHttpServer::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		dispatcher.registerRequestHandler(pattern, handler);
	}
    
	void AnotherHttpServer::unregisterRequestHandler(const string & pattern) {
		dispatcher.unregisterRequestHandler(pattern);
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
