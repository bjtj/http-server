#include "SimpleHttpRequestHandlerDispatcher.hpp"
#include <algorithm>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief SimpleHttpRequestHandlerDispatcher
	 */

	SimpleHttpRequestHandlerDispatcher::SimpleHttpRequestHandlerDispatcher() : sem(1) {
	}

	SimpleHttpRequestHandlerDispatcher::~SimpleHttpRequestHandlerDispatcher() {
	}

	void SimpleHttpRequestHandlerDispatcher::registerRequestHandler(const string & pattern, AutoRef<HttpRequestHandler> handler) {
		AutoLock _lock(sem);
		handlers.push_back(RequestHandlerNode(pattern, handler));
		sort(handlers.begin(), handlers.end(), _fn_sort_desc);
	}

	void SimpleHttpRequestHandlerDispatcher::unregisterRequestHandler(const string & pattern) {
		AutoLock _lock(sem);
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
		AutoLock _lock(sem);
		for (vector<RequestHandlerNode>::iterator iter = handlers.begin(); iter != handlers.end(); iter++) {
			RequestHandlerNode & node = *iter;
			if (node.patternMatch(path)) {
				return node.getHandler();
			}
		}
		return AutoRef<HttpRequestHandler>();
	}

	bool SimpleHttpRequestHandlerDispatcher::_fn_sort_desc(RequestHandlerNode a, RequestHandlerNode b) {
		return (a.pattern().size() > b.pattern().size());
	}

}
