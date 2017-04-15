#ifndef __SIMPLE_HTTP_REQUEST_HANDLER_DISPATCHER_HPP__
#define __SIMPLE_HTTP_REQUEST_HANDLER_DISPATCHER_HPP__

#include "HttpRequestHandler.hpp"
#include "HttpRequestHandlerDispatcher.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>

namespace HTTP {

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
			OS::AutoRef<HttpRequestHandler> handler;
		public:
			RequestHandlerNode(const std::string & pattern, OS::AutoRef<HttpRequestHandler> handler)
				: _pattern(pattern), handler(handler) {
			}
			virtual ~RequestHandlerNode() {
			}
			bool patternMatch(const std::string & query) {
				return UTIL::Text::match(_pattern, query);
			}
			bool equalsPattern(const std::string & pattern) {
				return (!_pattern.compare(pattern) ? true : false);
			}
			OS::AutoRef<HttpRequestHandler> getHandler() {
				return handler;
			}
			std::string & pattern() {
				return _pattern;
			}
		};
		std::vector<RequestHandlerNode> handlers;
		OS::Semaphore sem;
	public:
		SimpleHttpRequestHandlerDispatcher();
		virtual ~SimpleHttpRequestHandlerDispatcher();
		virtual void registerRequestHandler(const std::string & pattern, OS::AutoRef<HttpRequestHandler> handler);
		virtual void unregisterRequestHandler(const std::string & pattern);
		virtual OS::AutoRef<HttpRequestHandler> getRequestHandler(const std::string & path);
	private:
		static bool _fn_sort_desc(RequestHandlerNode a, RequestHandlerNode b);
	};

}

#endif
