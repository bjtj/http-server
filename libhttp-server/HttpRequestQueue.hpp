#ifndef __HTTP_REQUEST_QUEUE_HPP__
#define __HTTP_REQUEST_QUEUE_HPP__

#include <queue>
#include "HttpRequest.hpp"

namespace HTTP {

	/**
	 * @brief http request pool
	 */
	class HttpRequestQueue {
	private:
		int max;
		std::queue<HttpRequest> requestQueue;
		
	public:
		HttpRequestQueue(int max);
		virtual ~HttpRequestQueue();

		void request(HttpRequest & request);
		void poll();
	};
}

#endif
