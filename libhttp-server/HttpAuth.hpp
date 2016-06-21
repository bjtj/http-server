#ifndef __HTTP_AUTH_HPP__
#define __HTTP_AUTH_HPP__

#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace HTTP {
	
	class HttpAuth {
	public:
		HttpAuth();
		virtual ~HttpAuth();
		virtual bool validate(HttpRequest & request) = 0;
		virtual void setAuthentication(HttpResponse & response) = 0;
	};
}

#endif
