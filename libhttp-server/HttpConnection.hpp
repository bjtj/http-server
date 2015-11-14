#ifndef __HTTP_CONNECTION_HPP__
#define __HTTP_CONNECTION_HPP__

#include <liboslayer/os.hpp>
#include <string>
#include <map>
#include <vector>

#include "MultiConn.hpp"
#include "HttpHeader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "HttpHeaderReader.hpp"
#include "OnHttpRequestHandler.hpp"

namespace HTTP {

	/**
	 * @brief http connection
	 */
	class HttpConnection : public MultiConnProtocol, public OnHttpRequestHandler {
	private:
		HttpRequest * request;
		HttpResponse * response;
		HttpHeaderReader headerReader;
        OnHttpRequestHandler * requestHandler;
		
	public:
        HttpConnection(OnHttpRequestHandler * requestHandler);
		virtual ~HttpConnection();

		virtual void onClientConnect(MultiConn & server, ClientSession & client);
		virtual void onClientReceive(MultiConn & server, ClientSession & client, Packet & packet);
		virtual void onClientDisconnect(MultiConn & server, ClientSession & client);

		void prepareRequestAndResponse(ClientSession & client);

		virtual void onHttpRequest(HttpRequest & request, HttpResponse & response);

	private:
		void releaseRequest();
		void releaseResponse();
	};
}

#endif
