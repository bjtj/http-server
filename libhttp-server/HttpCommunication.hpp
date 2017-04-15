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

namespace HTTP {
	
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
		OS::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunication(OS::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunication();
		void reset();
		virtual bool isReadable();
		virtual bool isWritable();
        virtual void onConnected(OS::AutoRef<Connection> connection);
		virtual bool onReceivable(OS::AutoRef<Connection> connection);
		void readRequestHeaderIfNeed(OS::AutoRef<Connection> connection);
		void readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet);
		void onRequestHeader(HttpRequest & request, HttpResponse & response);
		void onHttpRequestContentCompleted(HttpRequest & request, OS::AutoRef<DataSink> sink, HttpResponse & response);
		void prepareRequestContentTransfer(HttpRequest & request, OS::AutoRef<DataSink> sink);
		virtual bool onWriteable(OS::AutoRef<Connection> connection);
		void sendResponseHeader(OS::AutoRef<Connection> connection);
		void sendResponseContent(OS::AutoRef<Connection> connection);
		virtual void onDisconnected(OS::AutoRef<Connection> connection);
		virtual bool isCommunicationCompleted();
		void handleError(HttpRequest & request, HttpResponse & response, int errorCode);
	};
}

#endif
