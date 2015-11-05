#include "HttpServer.hpp"
#include "MultiConnMultiplexServer.hpp"
#include "MultiConnThreadedServer.hpp"

namespace HTTP {

	using namespace OS;

	/**
	 * @brief http server thread
	 */
	HttpServerPollingThread::HttpServerPollingThread(HttpServer * server) :
		server(server), timeout(1000) {
	}
	HttpServerPollingThread::~HttpServerPollingThread() {
	}
	
	void HttpServerPollingThread::run() {
		while (!interrupted()) {
			server->poll(timeout);
		}
	}

	/**
	 * @brief http server
	 */
	HttpServer::HttpServer(int port) : port(port), conn(NULL), thread(NULL), useThreadedMultiConnType(true) {
	}
	HttpServer::~HttpServer() {
		stop();
	}

	MultiConn * HttpServer::createMultiConn(bool useThreadedMultiConnType) {
		MultiConn * conn = NULL;
		if (useThreadedMultiConnType) {
			conn = new MultiConnThreadedServer(port);
		} else {
			conn = new MultiConnMultiplexServer(port);
		}
		conn->setProtocol(this);
		return conn;
	}

	void HttpServer::startPollingThread() {
		if (!thread) {
			thread = new HttpServerPollingThread(this);
			thread->start();
		}
	}
	void HttpServer::stopPollingThread() {
		if (thread) {
			thread->interrupt();
			thread->join();
			delete thread;
			thread = NULL;
		}
	}

	void HttpServer::setUseThreadedMultiConnType(bool use) {
		this->useThreadedMultiConnType = use;
	}
	
	void HttpServer::start() {
		if (!conn) {
			conn = createMultiConn(useThreadedMultiConnType);
			conn->start();
		}
		
	}
	void HttpServer::startAsync() {
		start();
		startPollingThread();
	}
	void HttpServer::poll(unsigned long timeout_milli) {
		if (conn) {
			conn->poll(timeout_milli);
		}
	}
	void HttpServer::stop() {

		stopPollingThread();

		if (conn) {
			conn->stop();
			delete conn;
			conn = NULL;
		}
	}
	bool HttpServer::isRunning() {
		return (conn ? conn->isRunning() : false);
	}
}
