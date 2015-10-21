#include <liboslayer/Text.hpp>

#include "HttpProtocol.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief http protocol
	 */
	HttpProtocol::HttpProtocol() {
	}
	HttpProtocol::~HttpProtocol() {
	}
	
	void HttpProtocol::onConnect(MultiConn & server, ClientSession & client) {
		HttpConnection * conn = new HttpConnection(this);
		conns[client.getId()] = conn;

		conn->onConnect(server, client);
	}

	void HttpProtocol::onReceive(MultiConn & server, ClientSession & client, Packet & packet) {
		HttpConnection * conn = conns[client.getId()];
		if (conn) {
			conn->onReceive(server, client, packet);
		}
	}

	void HttpProtocol::onDisconnect(MultiConn & server, ClientSession & client) {

		HttpConnection * conn = conns[client.getId()];

		conn->onDisconnect(server, client);
		
		delete conn;
		conns.erase(client.getId());
	}
	
	string HttpProtocol::pathOnly(string unclearPath) {
		size_t f = unclearPath.find("?");
		if (f == string::npos) {
			return unclearPath;
		}
		return unclearPath.substr(0, f);
	}
	
	void HttpProtocol::vpath(string path, OnHttpRequestHandler * handler) {
		if (!handler) {
			handlers.erase(path);
		} else {
			handlers[path] = handler;
		}
	}
	
	OnHttpRequestHandler * HttpProtocol::getHandler(string path) {
		map<string, OnHttpRequestHandler*, vpath_comp>::iterator iter;
		string p = pathOnly(path);
		for (iter = handlers.begin(); iter != handlers.end(); iter++) {
			if (Text::match(iter->first, p)) {
				return iter->second;
			}
		}
		return NULL;
	}

	void HttpProtocol::onRequest(HttpRequest & request, HttpResponse & response) {
		OnHttpRequestHandler * handler = getHandler(request.getPath());
		if (handler) {
			handler->onRequest(request, response);
		} else {
			response.write("HTTP/1.1 404 not found\r\n\r\n");
		}
	}
}
