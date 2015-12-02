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
    
	void HttpRequestHandler::setFixedTransfer(HttpResponse & response, const string & content) {

		FixedTransfer * transfer = new FixedTransfer(content.length());
        ChunkedBuffer & cb = transfer->getChunkedBuffer();
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

		FileReader * reader = new FileReader(file);

		FileTransfer * transfer = new FileTransfer(reader, file.getSize());

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

	void SimpleHttpRequestHandlerDispatcher::registerRequestHandler(const string & pattern, HttpRequestHandler * handler) {
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

	HttpRequestHandler * SimpleHttpRequestHandlerDispatcher::getRequestHandler(const string & query) {
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end(); iter++) {
			RequestHandlerNode & node = *iter;
			if (node.patternMatch(query)) {
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
		connection.setReadSize(1);
	}

	void HttpCommunication::onDataReceived(Connection & connection, Packet & packet) {

		readRequestHeaderIfNeed(connection, packet);
		if (requestHeaderReader.complete()) {
            
            if (!requestHeaderHandled) {
                
                onRequestHeader(request, response);
                requestContentReadCounter.setContentSize(request.getContentLength());
                
            } else {
                
                AutoRef<DataTransfer> transfer = request.getTransfer();
				transfer->recv(packet);
                readRequestContent(request, response, packet);
                if (transfer->isCompleted()) {
                    onHttpRequestContentCompleted(request, response);
                    writeable = true;
                }
            }
		}
	}

	void HttpCommunication::readRequestHeaderIfNeed(Connection & connection, Packet & packet) {

		if (!requestHeaderReader.complete()) {
           
            requestHeaderReader.read(packet.getData(), (int)packet.getLength());

			packet.clear();

			if (requestHeaderReader.complete()) {
                request.setHeader(requestHeaderReader.getHeader());
				connection.resetReadLimit();
			}
		}
	}
    
    void HttpCommunication::readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
		HttpRequestHandler * handler = dispatcher->getRequestHandler(request.getPath());
		if (handler) {
			handler->onHttpRequestContent(request, response, packet);
		}
    }

	void HttpCommunication::onRequestHeader(HttpRequest & request, HttpResponse & response) {
        
        prepareRequestContentTransfer(request);
        
		HttpRequestHandler * handler = dispatcher->getRequestHandler(request.getPath());
		if (handler) {
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

		HttpRequestHandler * handler = dispatcher->getRequestHandler(request.getPath());
        if (handler) {
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
//        int writeLen =
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

	AnotherHttpServer::AnotherHttpServer(int port) : port(port), httpCommunicationMaker(&dispatcher), connectionManager(httpCommunicationMaker), thread(NULL) {
	}

	AnotherHttpServer::~AnotherHttpServer() {
	}

	void AnotherHttpServer::registerRequestHandler(const string & pattern, HttpRequestHandler * handler) {
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