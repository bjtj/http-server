#include <liboslayer/Text.hpp>
#include "HttpProtocol.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
    using namespace OS;

	/**
	 * @brief http protocol
	 */
    HttpProtocol::HttpProtocol() : handlersLock(1), connSem(1) {
        
		string msg404 = HttpStatusCodes::getMessage(404);
		string msg500 = HttpStatusCodes::getMessage(500);
		page404 = "<!DOCTYPE html><html><head><title>404 " + msg404 + "</title></head><body><h1>404 " + msg404 + "</h1><p>Page Not Found...</p></body></html>";
		page500 = "<!DOCTYPE html><html><head><title>500 " + msg500 + "</title></head><body><h1>500 " + msg500 + "</h1><p>Server Error...</p></body></html>";
        
	}
	HttpProtocol::~HttpProtocol() {
	}
	
	void HttpProtocol::onClientConnect(MultiConn & server, Connection & connection) {
        
        AutoLock lock(connSem);
        
		HttpConnection * conn = new HttpConnection(this);
		conns[connection.getId()] = conn;

		conn->onClientConnect(server, connection);
	}

	void HttpProtocol::onClientReceive(MultiConn & server, Connection & connection, Packet & packet) {
      
		HttpConnection * conn = conns[connection.getId()];
		if (conn) {
			conn->onClientReceive(server, connection, packet);
		}
	}
    
    void HttpProtocol::onClientWriteable(MultiConn &server, Connection &connection) {
        
        HttpConnection * conn = conns[connection.getId()];
        if (conn) {
            conn->onClientWriteable(server, connection);
        }
    }

	void HttpProtocol::onClientDisconnect(MultiConn & server, Connection & connection) {

        AutoLock lock(connSem);
        
		HttpConnection * conn = conns[connection.getId()];
        if (conn) {
            conn->onClientDisconnect(server, connection);
            delete conn;
        }
		conns.erase(connection.getId());
	}
	
	string HttpProtocol::pathOnly(string unclearPath) {
		size_t f = unclearPath.find("?");
		if (f == string::npos) {
			return unclearPath;
		}
		return unclearPath.substr(0, f);
	}
	
	void HttpProtocol::vpath(string path, OnHttpRequestHandler * requestHandler) {
        
        AutoLock lock(handlersLock);
        
		if (!requestHandler) {
			requestHandlers.erase(path);
		} else {
			requestHandlers[path] = requestHandler;
		}
	}
	
	OnHttpRequestHandler * HttpProtocol::getRequestHandler(string path) {
		map<string, OnHttpRequestHandler*, vpath_comp>::iterator iter;
		string p = pathOnly(path);
        
        AutoLock lock(handlersLock);
        
		for (iter = requestHandlers.begin(); iter != requestHandlers.end(); iter++) {
			if (Text::match(iter->first, p)) {
				return iter->second;
			}
		}
		return NULL;
	}

	void HttpProtocol::onHttpRequest(HttpRequest & request, HttpResponse & response) {
		OnHttpRequestHandler * handler = getRequestHandler(request.getPath());
		if (handler) {
			handler->onHttpRequest(request, response);
		} else {
			response.setStatusCode(404, HttpStatusCodes::getMessage(404));
			response.write(page404);
			response.setComplete();
		}
	}

	void HttpProtocol::setPage404(const std::string & html) {
		this->page404 = html;
	}

	void HttpProtocol::setPage500(const std::string & html) {
		this->page500 = html;
	}
}
