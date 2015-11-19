#ifndef __ANOTHER_HTTP_SERVER_HPP__
#define __ANOTHER_HTTP_SERVER_HPP__

#include "Connection.hpp"
#include "Communication.hpp"
#include "ConnectionManager.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {

	

	/**
	 * @brief HttpRequestHandler
	 */
	class HttpRequestHandler {
	private:
	public:

		HttpRequestHandler();
		virtual ~HttpRequestHandler();
    
		virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) = 0;
		virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) = 0;
        virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) = 0;

		void setFixedTransfer(HttpResponse & response, const std::string & content);
		void setFileTransfer(HttpResponse & response, const std::string & filepath);
		void setFileTransfer(HttpResponse & response, OS::File & file);
	};

	/**
	 * @brief HttpRequestHandlerDispatcher
	 */
	class HttpRequestHandlerDispatcher {
	private:
	public:

		HttpRequestHandlerDispatcher() {}
		virtual ~HttpRequestHandlerDispatcher() {}
		virtual void registerRequestHandler(const std::string & pattern, HttpRequestHandler * handler) = 0;
		virtual void unregisterRequestHandler(const std::string & pattern) = 0;
		virtual HttpRequestHandler * getRequestHandler(const std::string & query) = 0;
	};

	/**
	 * @brief SimpleHttpRequestHandlerDispatcher
	 */
	class SimpleHttpRequestHandlerDispatcher : public HttpRequestHandlerDispatcher {

	private:

		/**
		 * @brief
		 */
		class RequestHandlerNode {
		private:
			std::string pattern;
			HttpRequestHandler * handler;

		public:
			RequestHandlerNode(const std::string & pattern, HttpRequestHandler * handler) : pattern(pattern), handler(handler) {
			}
			virtual ~RequestHandlerNode() {
			}

			bool patternMatch(const std::string & query) {
				return UTIL::Text::match(pattern, query);
			}

			bool equalsPattern(const std::string & pattern) {
				return (!pattern.compare(pattern) ? true : false);
			}

			HttpRequestHandler * getHandler() {
				return handler;
			}
		};

		std::vector<RequestHandlerNode> handlers;

	public:

		SimpleHttpRequestHandlerDispatcher();
		virtual ~SimpleHttpRequestHandlerDispatcher();
		virtual void registerRequestHandler(const std::string & pattern, HttpRequestHandler * handler);
		virtual void unregisterRequestHandler(const std::string & pattern);
		virtual HttpRequestHandler * getRequestHandler(const std::string & query);
	};

	/**
	 * @brief HttpCommunication
	 */
	class HttpCommunication : public Communication {

	private:

		HttpRequest request;
		HttpHeaderReader requestHeaderReader;
		HttpResponse response;
		bool requestHeaderHandled;
		ReadCounter requestContentReadCounter;
		bool writeable;
		bool responseHeaderTransferDone;
		bool responseContentTransferDone;
		bool communicationCompleted;
    
		// DataTransfer * requestTransfer;
		HttpRequestHandlerDispatcher * dispatcher;

	public:

		HttpCommunication(HttpRequestHandlerDispatcher * dispatcher);
		virtual ~HttpCommunication();

		virtual void onConnected(Connection & connection);
		virtual void onDataReceived(Connection & connection, Packet & packet);
		void readRequestHeaderIfNeed(Connection & connection, Packet & packet);
		void readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet);
		void onRequestHeader(HttpRequest & request, HttpResponse & response);
		void prepareRequestContentTransfer(HttpRequest & request);
		virtual void onWriteable(Connection & connection);
		void sendResponseHeader(Connection & connection);
		void sendResponseContent(Connection & connection);
		virtual void onDisconnected(Connection & connection);
		virtual bool isCommunicationCompleted();
	};

	/**
	 * @brief HttpCommunicationMaker
	 */
	class HttpCommunicationMaker : public CommunicationMaker {

	private:

		HttpRequestHandlerDispatcher * dispatcher;

	public:

		HttpCommunicationMaker(HttpRequestHandlerDispatcher * dispatcher);
		virtual ~HttpCommunicationMaker();
    
		virtual Communication * makeCommunication();
	};

	/**
	 * @brief AnotherHttpServer
	 */
	class AnotherHttpServer {
	private:

		int port;

		SimpleHttpRequestHandlerDispatcher dispatcher;
		HttpCommunicationMaker httpCommunicationMaker;
		ConnectionManager connectionManager;
		OS::Thread * thread;

	public:

		AnotherHttpServer(int port);
		virtual ~AnotherHttpServer();

		void registerRequestHandler(const std::string & pattern, HttpRequestHandler * handler);
		void unregisterRequestHandler(const std::string & pattern);

		int getPort();

		void start();
		void startAsync();
		void poll(unsigned long timeout);
		void stop();
	};
}

#endif