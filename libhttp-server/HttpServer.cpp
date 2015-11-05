#include "HttpServer.hpp"
#include "MultiConnMultiplexServer.hpp"
#include "MultiConnThreadedServer.hpp"

namespace HTTP {

	using namespace OS;

	/**
	 * @brief http server thread
	 */
	HttpServerPollingThread::HttpServerPollingThread(HttpServer * server) :
		server(server) {
	}
	HttpServerPollingThread::~HttpServerPollingThread() {
	}
	
	void HttpServerPollingThread::run() {
		while (!interrupted()) {
			server->poll(1000);
		}
	}

	/**
	 * @brief http server
	 */
	HttpServer::HttpServer(int port) : thread(NULL) {
		conn = new MultiConnThreadedServer(port);
		//conn = new MultiConnMultiplexServer(port);
		conn->setProtocol(this);
	}
	HttpServer::~HttpServer() {
		delete conn;
	}
	
	void HttpServer::start() {
		conn->start();
	}
	void HttpServer::startAsync() {
		
		start();
		if (!thread) {
			thread = new HttpServerPollingThread(this);
			thread->start();
		}
	}
	void HttpServer::poll(unsigned long timeout_milli) {
		conn->poll(timeout_milli);
	}
	void HttpServer::stop() {
		if (thread) {
			thread->interrupt();
			thread->join();
			delete thread;
			thread = NULL;
		}
		conn->stop();
	}
	bool HttpServer::isRunning() {
		return conn->isRunning();
	}
}
