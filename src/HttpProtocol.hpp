#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

#include <vector>
#include <string>
#include "HttpHeader.hpp"
#include "MultiConn.hpp"

namespace HTTP {

	class HttpProtocol;

	class HttpRequest {
	private:
        HttpRequestHeader header;
		std::string content;
		OS::Socket & socket;
	public:
		HttpRequest(HttpHeader & header, OS::Socket & socket);
		virtual ~HttpRequest();

		std::string getMethod();
		std::string getPath();
		std::string getHeaderField(std::string & name);
		std::string getParameter(std::string & name);
		std::vector<std::string> getParameters(std::string & name);

		HttpRequestHeader & getHeader();
		void appendContent(char * buffer, size_t size);
		void clearContent();
	};

	class HttpResponse {
	private:
        OS::Socket & socket;
	public:
		HttpResponse(OS::Socket & socket);
		virtual ~HttpResponse();

		virtual int write(std::string content);
	};


	class HttpConnection : public MultiConnProtocol {
	private:
		HttpProtocol & protocol;
		HttpRequest * request;
		HttpResponse * response;
		HttpHeaderReader headerReader;
		
	public:
		HttpConnection(HttpProtocol & protocol);
		virtual ~HttpConnection();

		virtual void onConnect(MultiConnServer & server, OS::Socket & client);
		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client);

		virtual void gatherContent(char * buffer, size_t size);
		virtual void onRequest(HttpRequest & request, HttpResponse & response);

	private:
		virtual bool needMoreHeader();
		virtual void readHeader(Packet & packet);
		virtual bool needMoreContent();
		virtual void readContent(Packet & packet);
	};

	class HttpRequestHandler
	{
	public:
		HttpRequestHandler() {}
		virtual ~HttpRequestHandler() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;
	};


	class HttpProtocol : public MultiConnProtocol {
	private:
		std::map<OS::Socket*, HttpConnection*> conns;
		std::map<std::string, HttpRequestHandler*> handlers;
	public:
		HttpProtocol();
		virtual ~HttpProtocol();

		virtual void onConnect(MultiConnServer & server, OS::Socket & client);
		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client);

		void vpath(std::string path, HttpRequestHandler * handler);
		void onRequest(HttpRequest & request, HttpResponse & response);
		HttpRequestHandler * getHandler(std::string path);
	};
}

#endif
