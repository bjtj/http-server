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
#include "HttpServerConfig.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
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

		virtual UTIL::AutoRef<DataSink> getDataSink();
    
		virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response);
        virtual void onHttpRequestContentCompleted(HttpRequest & request, UTIL::AutoRef<DataSink> sink, HttpResponse & response);
		virtual void onHttpResponseHeaderCompleted(HttpRequest & request, HttpResponse & response);
		virtual void onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response);
		virtual bool onException(HttpRequest & request, HttpResponse & response, OS::Exception & ex);

		void setFixedTransfer(HttpResponse & response, const std::string & content);
		void setFileTransfer(HttpResponse & response, const std::string & filepath);
		void setFileTransfer(HttpResponse & response, OS::File & file);
		void setPartialFileTransfer(HttpResponse & response, OS::File & file, size_t start, size_t end);
	};

	/**
	 * @brief HttpRequestHandlerDispatcher
	 */
	class HttpRequestHandlerDispatcher {
	private:
	public:

		HttpRequestHandlerDispatcher() {}
		virtual ~HttpRequestHandlerDispatcher() {}
		virtual void registerRequestHandler(const std::string & pattern, UTIL::AutoRef<HttpRequestHandler> handler) = 0;
		virtual void unregisterRequestHandler(const std::string & pattern) = 0;
		virtual UTIL::AutoRef<HttpRequestHandler> getRequestHandler(const std::string & query) = 0;
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
			UTIL::AutoRef<HttpRequestHandler> handler;

		public:
			RequestHandlerNode(const std::string & pattern, UTIL::AutoRef<HttpRequestHandler> handler) : pattern(pattern), handler(handler) {
			}
			virtual ~RequestHandlerNode() {
			}

			bool patternMatch(const std::string & query) {
				return UTIL::Text::match(pattern, query);
			}

			bool equalsPattern(const std::string & pattern) {
				return (!pattern.compare(pattern) ? true : false);
			}

			UTIL::AutoRef<HttpRequestHandler> getHandler() {
				return handler;
			}
		};

		std::vector<RequestHandlerNode> handlers;

	public:

		SimpleHttpRequestHandlerDispatcher();
		virtual ~SimpleHttpRequestHandlerDispatcher();
		virtual void registerRequestHandler(const std::string & pattern, UTIL::AutoRef<HttpRequestHandler> handler);
		virtual void unregisterRequestHandler(const std::string & pattern);
		virtual UTIL::AutoRef<HttpRequestHandler> getRequestHandler(const std::string & path);
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
		bool writeable;
		bool responseHeaderTransferDone;
		bool responseContentTransferDone;
		bool communicationCompleted;
		UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunication(UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunication();

		void reset();
		virtual bool isReadable();
		virtual bool isWritable();
		virtual void onConnected(Connection & connection);
		virtual void onReceivable(Connection & connection);
		void readRequestHeaderIfNeed(Connection & connection);
		void readRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet);
		void onRequestHeader(HttpRequest & request, HttpResponse & response);
		void onHttpRequestContentCompleted(HttpRequest & request, UTIL::AutoRef<DataSink> sink, HttpResponse & response);
		void prepareRequestContentTransfer(HttpRequest & request, UTIL::AutoRef<DataSink> sink);
		virtual void onWriteable(Connection & connection);
		void sendResponseHeader(Connection & connection);
		void sendResponseContent(Connection & connection);
		virtual void onDisconnected(Connection & connection);
		virtual bool isCommunicationCompleted();
		void handleError(HttpRequest & request, HttpResponse & response, int errorCode);
	};

	/**
	 * @brief HttpCommunicationMaker
	 */
	class HttpCommunicationMaker : public CommunicationMaker {

	private:

		UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher;

	public:

		HttpCommunicationMaker(UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunicationMaker();
    
		virtual UTIL::AutoRef<Communication> makeCommunication();
	};

	/**
	 * @brief AnotherHttpServer
	 */
	class AnotherHttpServer {
	private:

		HttpServerConfig config;
		UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
		ConnectionManager _connectionManager;
		OS::Thread * thread;

	public:

		AnotherHttpServer(HttpServerConfig config);
        AnotherHttpServer(HttpServerConfig config, UTIL::AutoRef<ServerSocketMaker> serverSocketMaker);
		virtual ~AnotherHttpServer();

		void registerRequestHandler(const std::string & pattern, UTIL::AutoRef<HttpRequestHandler> handler);
		void unregisterRequestHandler(const std::string & pattern);
		int getPort();
		void start();
		void startAsync();
		void poll(unsigned long timeout);
		void stop();
		size_t connections();
        ConnectionManager & connectionManager();
	};
}

#endif
