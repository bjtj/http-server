#ifndef __HTTP_REQUEST_POOL_HPP__
#define __HTTP_REQUEST_POOL_HPP__

#include "HttpRequest.hpp"

namespace HTTP {

	/**
	 * @brief http request pool
	 */
	class HttpRequestPool {
	private:
		int max;
	public:
		HttpRequestPool(int max);
		virtual ~HttpRequestPool();

		void request(HttpRequest & request);
	};

}

#endif
