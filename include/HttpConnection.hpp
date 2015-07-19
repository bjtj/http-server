#ifndef __HTTP_CONNECTION_HPP__
#define __HTTP_CONNECTION_HPP__

#include <string>
#include "os.hpp"
#include "MultiConn.hpp"
#include "HttpHeader.hpp"

namespace HTTP {

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
		std::string getParameter(const std::string & name);
		std::string getParameter(const char * name);
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
		bool headerSent;
		int contentLength;
	public:
		HttpResponse(OS::Socket & socket);
		virtual ~HttpResponse();

		void setStatusCode(int code);
		void setStatusCode(int code, std::string message);
		void setParts(std::vector<std::string> &parts);
		void setContentLength(int length);
		void setContentType(std::string type);

		void clearBuffer();
		int send(const char * buf, int size);
		int write(const std::string & content);
		int write(const char * buf, int size);
		void sendHeaderOnce();
		void sendContent();
		void setComplete();
		bool hasComplete();
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
	 * @brief http request handler
	 */
	class HttpRequestHandlerDecorator {
	private:
        HttpRequestHandler * handler;
	public:
		HttpRequestHandlerDecorator(HttpRequestHandler * handler) : handler(handler) {}
		virtual ~HttpRequestHandlerDecorator() {}

		virtual void onRequest(HttpRequest & request, HttpResponse & response) = 0;

		HttpRequestHandler * getHandler() {return handler;}
	};
	
	/**
	 * @brief http connection
	 */
	class HttpConnection : public MultiConnProtocol, public HttpRequestHandlerDecorator {
	private:
		HttpRequest * request;
		HttpResponse * response;
		HttpHeaderReader headerReader;
		
	public:
		HttpConnection(HttpRequestHandler * handler);
		virtual ~HttpConnection();

		virtual void onConnect(MultiConn & server, OS::Socket & client);
		virtual void onReceive(MultiConn & server, OS::Socket & client, Packet & packet);
		virtual void onDisconnect(MultiConn & server, OS::Socket & client);

		virtual void readContent(char * buffer, size_t size);
		virtual void onRequest(HttpRequest & request, HttpResponse & response);

	private:
		void releaseRequest();
		void releaseResponse();
	};
}

#endif
