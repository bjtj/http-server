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

		virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) = 0;
	};

	/**
	 * @brief http request handler
	 */
	class OnHttpRequestHandlerDecorator : public OnHttpRequestHandler {
	private:
        OnHttpRequestHandler * handler;
	public:
		OnHttpRequestHandlerDecorator(OnHttpRequestHandler * handler) : handler(handler) {}
		virtual ~OnHttpRequestHandlerDecorator() {}

		virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) = 0;

		OnHttpRequestHandler * getHandler() {return handler;}
	};
}

#endif
