#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

#include <vector>
#include <string>
#include "HttpHeader.hpp"
#include "MultiConn.hpp"

namespace HTTP {
	

	class HttpRequest {
	private:
        HttpHeader header;
	public:
		HttpRequest();
		virtual ~HttpRequest();

		std::string & getMethod();
		std::string & getPath();
		std::string & getParamter(std::string & name);
		std::vector<std::string> & getParamters(std::string & name);
		
	};

	class HttpResponse {
	public:
		HttpResponse();
		virtual ~HttpResponse();

		virtual int write(std::string & content);
	};


	class HttpConnection : public MultiConnProtocol {
	private:
        HttpRequest request;
		HttpResponse response;

		std::string buffer;
		
	public:
		HttpConnection();
		virtual ~HttpConnection();

		virtual void onConnect(MultiConnServer & server, OS::Socket & client);
		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client);

	};


	class HttpProtocol : public MultiConnProtocol {
	private:
		std::map<OS::Socket*, HttpConnection*> conns;
	public:
		HttpProtocol();
		virtual ~HttpProtocol();

		virtual void onConnect(MultiConnServer & server, OS::Socket & client);
		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client);
	};

}

#endif
