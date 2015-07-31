#ifndef __HTTP_CONNECTION_HPP__
#define __HTTP_CONNECTION_HPP__

#include <liboslayer/os.hpp>
#include <string>
#include <map>
#include <vector>

#include "MultiConn.hpp"
#include "HttpHeader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "HttpHeaderReader.hpp"
#include "OnHttpRequestHandler.hpp"

namespace HTTP {

	/**
	 * @brief http connection
	 */
	class HttpConnection : public MultiConnProtocol, public HttpRequestHandlerDecorator {
	private:
		HttpRequest * request;
		HttpResponse * response;
		HttpHeaderReader headerReader;
		
	public:
		HttpConnection(HttpRequestHandler * handler);
		virtual ~HttpConnection();

		virtual void onConnect(MultiConn & server, OS::Socket & client);
		virtual void onReceive(MultiConn & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConn & server, OS::Socket & client);

		virtual void readContent(char * buffer, size_t size);
		virtual void onRequest(HttpRequest & request, HttpResponse & response);

	private:
		void releaseRequest();
		void releaseResponse();
	};
}

#endif
