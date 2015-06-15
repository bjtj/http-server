#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

#include <vector>
#include <string>
#include "HttpHeader.hpp"
#include "MultiConn.hpp"

namespace HTTP {


	class HttpProtocol;

	/**
	 * @brief http request
	 */
	class HttpRequest {
	private:
        HttpRequestHeader header;
		std::string content;
		int contentSize;
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
		void compactContent();
		std::string & getContent();

		int getContentLength();
		std::string getContentType();
		bool remaining();
	};

	/**
	 * @brief http response
	 */
	class HttpResponse {
	private:
		HttpResponseHeader header;
        OS::Socket & socket;
		bool complete;
		std::string content;
	public:
		HttpResponse(OS::Socket & socket);
		virtual ~HttpResponse();

		void setStatusCode(int code);
		void setStatusCode(int code, std::string message);
		void setParts(std::vector<std::string> &parts);
		void setContentLength(int length);
		void setContentType(std::string type);
		
		int write(std::string content);
		int write(char * buf, int size);
		void sendContent();
		void setComplete();
		bool hasComplete();
	};


	/**
	 * @brief http connection
	 */
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
		bool needMoreHeader();
		void readHeader(Packet & packet);
		bool needMoreContent();
		void readContent(Packet & packet);

		void releaseRequest();
		void releaseResponse();
	};

	/**
	 * @brief http request handler
	 */
	class HttpRequestHandler {
	public:
		HttpRequestHandler() {}
		virtual ~HttpRequestHandler() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;
	};


	/**
	 * @brief http protocol
	 */
	class HttpProtocol : public MultiConnProtocol {
	private:
		std::map<OS::Socket*, HttpConnection*> conns;
		std::map<std::string, HttpRequestHandler*> handlers;
		HttpRequest * request;
		HttpResponse * response;
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
