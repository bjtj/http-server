#include "HttpProtocol.hpp"
#include "HttpStatusCodes.hpp"
#include "Text.hpp"

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
	
	void HttpProtocol::onConnect(MultiConn & server, OS::Socket & client) {
		conns[&client] = new HttpConnection(this);
	}

	void HttpProtocol::onReceive(MultiConn & server, OS::Socket & client, Packet & packet) {
		HttpConnection * conn = conns[&client];
		if (conn) {
			conn->onReceive(server, client, packet);
		}
	}

	void HttpProtocol::onDisconnect(MultiConn & server, OS::Socket & client) {
		delete conns[&client];
		conns.erase(&client);
	}
	string HttpProtocol::pathOnly(string unclearPath) {
		size_t f = unclearPath.find("?");
		if (f == string::npos) {
			return unclearPath;
		}
		return unclearPath.substr(0, f);
	}
	void HttpProtocol::vpath(string path, HttpRequestHandler * handler) {
		if (!handler) {
			handlers.erase(path);
		} else {
			handlers[path] = handler;
		}
	}
	
	HttpRequestHandler * HttpProtocol::getHandler(string path) {
		map<string, HttpRequestHandler*>::iterator iter;
		string p = pathOnly(path);
		for (iter = handlers.begin(); iter != handlers.end(); iter++) {
			if (Text::match(iter->first, p)) {
				return iter->second;
			}
		}
		return NULL;
	}

	void HttpProtocol::onRequest(HttpRequest & request, HttpResponse & response) {
		HttpRequestHandler * handler = getHandler(request.getPath());
		if (handler) {
			handler->onRequest(request, response);
		} else {
			response.write("HTTP/1.1 404 not found\r\n\r\n");
		}
	}
}
