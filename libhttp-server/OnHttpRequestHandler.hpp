#ifndef __ON_HTTP_REQUEST_HANDLER_HPP__
#define __ON_HTTP_REQUEST_HANDLER_HPP__

#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace HTTP {

	/**
	 * @brief http request handler
	 */
	class HttpRequestHandler {
	public:
		HttpRequestHandler() {}
		virtual ~HttpRequestHandler() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;
	};

	/**
	 * @brief http request handler
	 */
	class HttpRequestHandlerDecorator {
	private:
        HttpRequestHandler * handler;
	public:
		HttpRequestHandlerDecorator(HttpRequestHandler * handler) : handler(handler) {}
		virtual ~HttpRequestHandlerDecorator() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;

		HttpRequestHandler * getHandler() {return handler;}
	};
}

#endif
