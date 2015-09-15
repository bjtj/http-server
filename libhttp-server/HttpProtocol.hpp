#ifndef __HTTP_PROTOCOL_HPP__
#define __HTTP_PROTOCOL_HPP__

#include <vector>
#include <string>
#include <map>

#include "MultiConn.hpp"
#include "HttpHeader.hpp"
#include "OnHttpRequestHandler.hpp"
#include "HttpConnection.hpp"

namespace HTTP {

	/**
	 * @brief vpath comparator
	 */
	struct vpath_comp {
		bool operator() (const std::string & lhs, const std::string & rhs) const {
			return lhs > rhs;
		}
	};

	/**
	 * @brief http protocol
	 */
	class HttpProtocol : public MultiConnProtocol, public OnHttpRequestHandler {
	private:
		std::map<int, HttpConnection*> conns;
		std::map<std::string, OnHttpRequestHandler*, vpath_comp> handlers;
		HttpRequest * request;
		HttpResponse * response;
		
	public:
		HttpProtocol();
		virtual ~HttpProtocol();

		virtual void onConnect(MultiConn & server, ClientSession & client);
		virtual void onReceive(MultiConn & server, ClientSession & client, Packet & packet);
		virtual void onDisconnect(MultiConn & server, ClientSession & client);

		std::string pathOnly(std::string unclearPath);
		void vpath(std::string path, OnHttpRequestHandler * handler);
		OnHttpRequestHandler * getHandler(std::string path);

		virtual void onRequest(HttpRequest & request, HttpResponse & response);
	};
}

#endif
