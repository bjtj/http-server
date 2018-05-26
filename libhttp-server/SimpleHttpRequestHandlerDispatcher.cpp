#include "SimpleHttpRequestHandlerDispatcher.hpp"
#include <algorithm>
#include <liboslayer/AutoLock.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/Text.hpp>

namespace http {

	using namespace std;
	using namespace osl;

	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));


	SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::RequestHandlerNode(const string & pattern, AutoRef<HttpRequestHandler> handler)
		: _pattern(pattern), handler(handler) {
	}
	
	SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::~RequestHandlerNode() {
	}
	
	bool SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::patternMatch(const string & query) {
		return Text::match(_pattern, query);
	}
	
	bool SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::equalsPattern(const string & pattern) {
		return (!_pattern.compare(pattern) ? true : false);
	}
	
	AutoRef<HttpRequestHandler> SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::getHandler() {
		return handler;
	}
	
	string & SimpleHttpRequestHandlerDispatcher::RequestHandlerNode::pattern() {
		return _pattern;
	}

	/**
	 * @brief SimpleHttpRequestHandlerDispatcher
	 */

	SimpleHttpRequestHandlerDispatcher::SimpleHttpRequestHandlerDispatcher() : sem(1) {
	}

	SimpleHttpRequestHandlerDispatcher::~SimpleHttpRequestHandlerDispatcher() {
	}

	void SimpleHttpRequestHandlerDispatcher::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		AutoLock _lock((Ref<Semaphore>(&sem)));
		logger->info("Register request handler - '" + pattern + "'");
		handlers.push_back(RequestHandlerNode(pattern, handler));
		sort(handlers.begin(), handlers.end(), _fn_sort_desc);
	}

	void SimpleHttpRequestHandlerDispatcher::unregisterRequestHandler(const string & pattern) {
		AutoLock _lock((Ref<Semaphore>(&sem)));
		logger->info("Unregister request handler - '" + pattern + "'");
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end();) {
			RequestHandlerNode & node = *iter;
			if (node.equalsPattern(pattern)) {
				iter = handlers.erase(iter);
			} else {
				iter++;
			}
		}
	}

	AutoRef<HttpRequestHandler> SimpleHttpRequestHandlerDispatcher::getRequestHandler(const string & path) {
		AutoLock _lock((Ref<Semaphore>(&sem)));
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end(); iter++) {
			RequestHandlerNode & node = *iter;
			if (node.patternMatch(path)) {
				return node.getHandler();
			}
		}
		logger->info("pattern not found - '" + path + "'");
		return AutoRef<HttpRequestHandler>();
	}

	bool SimpleHttpRequestHandlerDispatcher::_fn_sort_desc(RequestHandlerNode a, RequestHandlerNode b) {
		return (a.pattern().size() > b.pattern().size());
	}

}
