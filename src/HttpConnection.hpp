#ifndef __HTTP_CONNECTION_HPP__
#define __HTTP_CONNECTION_HPP__

namespace HTTP {
	
	class HttpConnection {
	public:
		HttpConnection();
		virtual ~HttpConnection();
	};


	class HttpRequest {
	private:
	public:
		HttpRequest();
		virtual ~HttpRequest();
	};

	class HttpResponse {
	public:
		HttpResponse();
		virtual ~HttpResponse();
	};
	
}

#endif
