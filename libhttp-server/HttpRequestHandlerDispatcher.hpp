#ifndef __HTTP_REQUEST_HANDLER_DISPATCHER_HPP__
#define __HTTP_REQUEST_HANDLER_DISPATCHER_HPP__

#include "HttpRequestHandler.hpp"
#include <liboslayer/AutoRef.hpp>

namespace http {
	
	/**
	 * @brief HttpRequestHandlerDispatcher
	 */
	class HttpRequestHandlerDispatcher {
	private:
	public:
		HttpRequestHandlerDispatcher() {}
		virtual ~HttpRequestHandlerDispatcher() {}
		virtual void registerRequestHandler(const std::string & pattern, osl::AutoRef<HttpRequestHandler> handler) = 0;
		virtual void unregisterRequestHandler(const std::string & pattern) = 0;
		virtual osl::AutoRef<HttpRequestHandler> getRequestHandler(const std::string & query) = 0;
	};
}

#endif
