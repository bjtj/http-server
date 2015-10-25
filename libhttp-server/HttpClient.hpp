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
    
    class HttpResponseDump {
    private:
    public:
        HttpResponseDump();
        virtual ~HttpResponseDump();
        std::string dump(HttpHeader & responseHeader, OS::Socket & socket);
    };
    
	/**
	 * @brief http response handler
	 * it'll be set to http client
	 */
	class HttpResponseHandler {
	private:
	public:
		HttpResponseHandler() {}
		virtual ~HttpResponseHandler() {}

		virtual void onResponse(HttpClient & httpClient, HttpHeader & responseHeader, OS::Socket & socket) = 0;
	};

	/**
	 * @brief http client
	 */
	class HttpClient {
	private:
		std::string httpProtocol;
		HttpResponseHandler * responseHandler;
		OS::Socket * socket;
		std::map<std::string, std::string> defaultHeaderFields;
		bool followRedirect;
		
	public:
		HttpClient();
		virtual ~HttpClient();

		void setFollowRedirect(bool followRedirect);
		void setHttpResponseHandler(HttpResponseHandler * responseHandler);
		void request(Url & url);
		void request(Url & url, std::string method, char * data, int len);
		void request(Url & url, std::string method,
					 std::map<std::string, std::string> & additionalHeaderFields,
					 char * data, int len);
		OS::Socket *  connect(Url & url);
		void disconnect(OS::Socket * socket);

	private:
		HttpHeader makeRequestHeader(std::string method,
									 std::string path,
									 std::string protocol,
									 std::string targetHost);
		void sendRequestPacket(OS::Socket & socket, HttpHeader & header, char * buffer, int len);
		HttpHeader readResponseHeader(OS::Socket & socket);
		bool checkIf302(HttpHeader & responseHeader);
		int consume(OS::Socket & socket, int length);
		HttpHeader processRedirect(OS::Socket & socket,
								   HttpHeader requestHeader,
								   HttpHeader responseHeader,
								   char * data,
								   int len);
	};
}

#endif
