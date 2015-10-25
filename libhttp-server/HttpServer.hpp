#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

#include <liboslayer/os.hpp>
#include "MultiConn.hpp"

#include "HttpParameter.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderParser.hpp"
#include "HttpHeaderReader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "OnHttpRequestHandler.hpp"
#include "HttpConnection.hpp"
#include "HttpProtocol.hpp"
#include "HttpServer.hpp"
#include "Url.hpp"

namespace HTTP {

	class HttpServer;

	/**
	 * @brief server thread
	 */
	class HttpServerPollingThread : public OS::Thread {
	private:
		HttpServer * server;
		
	public:
		HttpServerPollingThread(HttpServer * server);
		virtual ~HttpServerPollingThread();
		virtual void run();
	};

	/**
	 * @brief http server
	 */
	class HttpServer : public HttpProtocol {
	private:
		MultiConn * conn;
		HttpServerPollingThread * thread;
	public:
		HttpServer(int port);
		virtual ~HttpServer();

		virtual void start();
		virtual void startAsync();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();
	};

}

#endif
