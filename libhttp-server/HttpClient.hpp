#ifndef __HTTP_CLIENT_HPP__
#define __HTTP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include <map>
#include <string>
#include "Url.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"

namespace HTTP {

	class HttpClient;

	class HttpResponseHandler {
	private:
	public:
		HttpResponseHandler() {}
		virtual ~HttpResponseHandler() {}

		virtual void onResponse(HttpClient & httpClient, HttpHeader & responseHeader, OS::Socket & socket) = 0;
	};
	
	class HttpClient {
	private:
		HttpHeaderReader reader;
		HttpResponseHandler * responseHandler;
		OS::Socket * socket;
		std::map<std::string, std::string> defaultHeaderFields;
		
	public:
		HttpClient();
		virtual ~HttpClient();

		void setHttpResponseHandler(HttpResponseHandler * responseHandler);
		void request(Url & url);
		OS::Socket *  connect(Url & url);
		void disconnect(OS::Socket * socket);

	private:
		HttpHeader makeRequestHeader(std::string method, std::string path, std::string protocol);
		void sendRequestPacket(OS::Socket & socket, HttpHeader & header, char * buffer, int len);
		HttpHeader readResponseHeader(OS::Socket & socket);
		
	};
}

#endif
