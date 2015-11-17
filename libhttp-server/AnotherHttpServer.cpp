#include "AnotherHttpServer.hpp"

#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();


	/**
	 * @brief FixedTransfer
	 */

    FixedTransfer::FixedTransfer() {
    }
    FixedTransfer::~FixedTransfer() {
    }
    ChunkedBuffer & FixedTransfer::getChunkedBuffer() {
        return chunkedBuffer;
    }
    void FixedTransfer::recv(Packet & packet) {
        chunkedBuffer.write(packet.getData(), packet.getLength());
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }
    void FixedTransfer::send(Connection & connection) {
        if (chunkedBuffer.remain()) {
            char buffer[1024] = {0,};
            size_t len = chunkedBuffer.read(buffer, sizeof(buffer));
            connection.send(buffer, len);
        }
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }

	string FixedTransfer::getString() {
		return string(chunkedBuffer.getChunkData(), chunkedBuffer.getChunkSize());
	}

	/**
	 * @brief HttpRequestHandler
	 */

	HttpRequestHandler::HttpRequestHandler() {
	}
	HttpRequestHandler::~HttpRequestHandler() {
	}
    
	void HttpRequestHandler::setFixedTrnasfer(HttpResponse & response, const string & content) {
		FixedTransfer * transfer = new FixedTransfer;
        ChunkedBuffer & cb = transfer->getChunkedBuffer();
        cb.setChunkSize(content.length());
        cb.write(content.c_str(), content.length());
        cb.resetPosition();
        
        response.setTransfer(transfer);
		response.setContentLength((int)content.length());
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
                
                onRequestHeader(request);
                requestContentReadCounter.setContentSize(request.getContentLength());
                
            } else {
                
				DataTransfer * transfer = request.getTransfer();
				transfer->recv(packet);
                readRequestContent(request, packet);
                if (transfer->isCompleted()) {
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
    
    void HttpCommunication::readRequestContent(HttpRequest & request, Packet & packet) {
		HttpRequestHandler * handler = dispatcher->getRequestHandler(request.getPath());
		if (handler) {
			handler->onHttpRequestContent(request, packet);
		}
    }

	void HttpCommunication::onRequestHeader(HttpRequest & request) {
        
        prepareRequestContentTransfer(request);
        
		HttpRequestHandler * handler = dispatcher->getRequestHandler(request.getPath());
		if (handler) {
			handler->onHttpRequest(request, response);
		}

		if (!request.getTransfer()) {
			writeable = true;
		}
        
        requestHeaderHandled = true;
	}

	void HttpCommunication::prepareRequestContentTransfer(HttpRequest & request) {

		if (request.getHeader().isChunkedTransfer()) {
			logger.logv("choose chunked tranfer");
			request.setTransfer(new ChunkedTransfer);
        } else {
			logger.logv("choose fixed tranfer");
            if (request.getContentLength() > 0) {
                FixedTransfer * transfer = new FixedTransfer;
                ChunkedBuffer & cb = transfer->getChunkedBuffer();
                cb.setChunkSize(request.getContentLength());
				request.setTransfer(transfer);
            }
        }

	}

	void HttpCommunication::onWriteable(Connection & connection) {

		if (!writeable) {
			return;
		}

		logger.logv("write something...");

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
            
        if (!header.isChunkedTransfer() && header.getContentLength() == 0) {
            responseContentTransferDone = true;
        }
            
        responseHeaderTransferDone = true;
	}

	void HttpCommunication::sendResponseContent(Connection & connection) {

		DataTransfer * responseTransfer = response.getTransfer();

		if (responseTransfer) {
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