#ifndef __HTTP_COMMUNICATION_HPP__
#define __HTTP_COMMUNICATION_HPP__

#include "Connection.hpp"
#include "Communication.hpp"
#include "ConnectionManager.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"
#include "HttpServerConfig.hpp"
#include "HttpRequestHandler.hpp"
#include "HttpRequestHandlerDispatcher.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>

namespace http {
	
	/**
	 * @brief HttpCommunication
	 */
	class HttpCommunication : public Communication {
	private:
		HttpRequest request;
		HttpHeaderReader requestHeaderReader;
		HttpResponse response;
		bool requestHeaderHandled;
		bool writeable;
		bool responseHeaderTransferDone;
		bool responseContentTransferDone;
		bool communicationCompleted;
		osl::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunication(osl::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunication();
		void reset();
		virtual bool isReadable();
		virtual bool isWritable();
        virtual void onConnected(osl::AutoRef<Connection> connection);
		virtual bool onReceivable(osl::AutoRef<Connection> connection);
		void readRequestHeaderIfNeed(osl::AutoRef<Connection> connection);
		void readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet);
		void onRequestHeader(HttpRequest & request, HttpResponse & response);
		void onHttpRequestContentCompleted(HttpRequest & request, osl::AutoRef<DataSink> sink, HttpResponse & response);
		void prepareRequestContentTransfer(HttpRequest & request, osl::AutoRef<DataSink> sink);
		virtual bool onWriteable(osl::AutoRef<Connection> connection);
		void sendResponseHeader(osl::AutoRef<Connection> connection);
		void sendResponseContent(osl::AutoRef<Connection> connection);
		virtual void onDisconnected(osl::AutoRef<Connection> connection);
		virtual bool isCommunicationCompleted();
		void handleError(HttpRequest & request, HttpResponse & response, int errorCode);
	};
}

#endif
