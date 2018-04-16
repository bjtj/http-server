#include "HttpCommunication.hpp"
#include "StringDataSink.hpp"
#include "StringDataSource.hpp"
#include "DataTransfer.hpp"
#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include <liboslayer/Logger.hpp>
#include <liboslayer/File.hpp>


namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));

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
		logger->debug("[CONNECTED] '" + connection->getRemoteAddress().getHost() + "'");
	}

	bool HttpCommunication::isReadable() {
		return writeable == false;
	}
	
	bool HttpCommunication::isWritable() {
		return writeable == true;
	}

	bool HttpCommunication::onReceivable(AutoRef<Connection> connection) {
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

	void HttpCommunication::readRequestHeaderIfNeed(AutoRef<Connection> connection) {
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
        if (transfer.nil()) {
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
		if (request.isChunkedTransfer()) {
			request.setTransfer(AutoRef<DataTransfer>(new ChunkedTransfer(sink)));
        } else {
            if (request.getContentLength() > 0) {
                AutoRef<DataTransfer> transfer(new FixedTransfer(sink, request.getContentLength()));
				request.setTransfer(transfer);
            }
        }
	}

	bool HttpCommunication::onWriteable(AutoRef<Connection> connection) {
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
			if (request.getProtocol() == "HTTP/1.1" &&
				Text::equalsIgnoreCase(request.getHeaderFieldIgnoreCase("Connection"), "close") == false &&
				Text::equalsIgnoreCase(response.getHeaderFieldIgnoreCase("Connection"), "close") == false) {
				reset();
			}
		}
		return true;
	}

	void HttpCommunication::sendResponseHeader(AutoRef<Connection> connection) {
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

	void HttpCommunication::sendResponseContent(AutoRef<Connection> connection) {
        AutoRef<DataTransfer> responseTransfer = response.getTransfer();
		if (!responseTransfer.nil()) {
			responseTransfer->send(connection);
			if (responseTransfer->completed()) {
				responseContentTransferDone = true;
			}
		} else {
			responseContentTransferDone = true;
		}
	}

	void HttpCommunication::onDisconnected(AutoRef<Connection> connection) {
		logger->debug("[DISCONNECTED] '" + connection->getRemoteAddress().getHost() + "'");
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

}
