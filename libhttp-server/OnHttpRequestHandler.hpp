#ifndef __ON_HTTP_REQUEST_HANDLER_HPP__
#define __ON_HTTP_REQUEST_HANDLER_HPP__

#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace HTTP {

	/**
	 * @brief http request handler
	 */
	class OnHttpRequestHandler {
	public:
		OnHttpRequestHandler() {}
		virtual ~OnHttpRequestHandler() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;
	};

	/**
	 * @brief http request handler
	 */
	class OnHttpRequestHandlerDecorator : public OnHttpRequestHandler {
	private:
        OnHttpRequestHandler * handler;
	public:
		OnHttpRequestHandlerDecorator(HttpRequestHandler * handler) : handler(handler) {}
		virtual ~OnHttpRequestHandlerDecorator() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;

		OnHttpRequestHandler * getHandler() {return handler;}
	};
}

#endif
