#ifndef __SIMPLE_HTTP_REQUEST_HANDLER_DISPATCHER_HPP__
#define __SIMPLE_HTTP_REQUEST_HANDLER_DISPATCHER_HPP__

#include "HttpRequestHandler.hpp"
#include "HttpRequestHandlerDispatcher.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>

namespace http {

	/**
	 * @brief SimpleHttpRequestHandlerDispatcher
	 */
	class SimpleHttpRequestHandlerDispatcher : public HttpRequestHandlerDispatcher {
	private:
	    /**
		 * @brief request handler node
		 */
		class RequestHandlerNode {
		private:			
			std::string _pattern;
			osl::AutoRef<HttpRequestHandler> handler;
		public:
			RequestHandlerNode(const std::string & pattern, osl::AutoRef<HttpRequestHandler> handler);
			virtual ~RequestHandlerNode();
			bool patternMatch(const std::string & query);
			bool equalsPattern(const std::string & pattern);
			osl::AutoRef<HttpRequestHandler> getHandler();
			std::string & pattern();
		};
	private:
		std::vector<RequestHandlerNode> handlers;
		osl::Semaphore sem;
	public:
		SimpleHttpRequestHandlerDispatcher();
		virtual ~SimpleHttpRequestHandlerDispatcher();
		virtual void registerRequestHandler(const std::string & pattern, osl::AutoRef<HttpRequestHandler> handler);
		virtual void unregisterRequestHandler(const std::string & pattern);
		virtual osl::AutoRef<HttpRequestHandler> getRequestHandler(const std::string & path);
	private:
		static bool _fn_sort_desc(RequestHandlerNode a, RequestHandlerNode b);
	};

}

#endif
