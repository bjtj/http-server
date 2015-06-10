#include "HttpServer.hpp"


namespace HTTP {
	
	HttpServer::HttpServer(int port) : port(port), running(false) {
	}
	HttpServer::~HttpServer() {
	}
	
	void HttpServer::start() {
	}
	void HttpServer::stop() {
	}
	bool HttpServer::isRunning() {
		return running;
	}	
	
}
