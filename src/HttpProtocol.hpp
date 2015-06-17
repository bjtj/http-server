#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

#include <vector>
#include <string>
#include "HttpHeader.hpp"
#include "MultiConn.hpp"
#include "HttpConnection.hpp"

namespace HTTP {

	/**
	 * @brief http protocol
	 */
	class HttpProtocol : public MultiConnProtocol, public HttpRequestHandler {
	private:
		std::map<OS::Socket*, HttpConnection*> conns;
		std::map<std::string, HttpRequestHandler*> handlers;
		HttpRequest * request;
		HttpResponse * response;
	public:
		HttpProtocol();
		virtual ~HttpProtocol();

		virtual void onConnect(MultiConn & server, OS::Socket & client);
		virtual void onReceive(MultiConn & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConn & server, OS::Socket & client);

		void vpath(std::string path, HttpRequestHandler * handler);
		HttpRequestHandler * getHandler(std::string path);

		virtual void onRequest(HttpRequest & request, HttpResponse & response);
	};
}

#endif
