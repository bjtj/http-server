#include "HttpRequestQueue.hpp"

namespace HTTP {
	
	HttpRequestQueue::HttpRequestQueue(int max) : max(max) {
	}

	HttpRequestQueue::~HttpRequestQueue() {
	}

	void HttpRequestQueue::request(HttpRequest & request) {
		requestQueue.push(request);
	}

	void HttpRequestQueue::poll() {
		// TODO: poll
	}
}
