#include <iostream>
#include <liboslayer/Text.hpp>

#include "HttpConnection.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
    using namespace OS;
	using namespace UTIL;

	
	/**
	 * @brief http connection constructor
	 */
    HttpConnection::HttpConnection(OnHttpRequestHandler * handler)
		: OnHttpRequestHandlerDecorator(handler), request(NULL), response(NULL) {
	}
	HttpConnection::~HttpConnection() {
		releaseRequest();
		releaseResponse();
	}

	void HttpConnection::onClientConnect(MultiConn & server, ClientSession & client) {
		client.setBufferSize(1);
	}

	void HttpConnection::onClientReceive(MultiConn & server, ClientSession & client, Packet & packet) {

		if (!headerReader.complete()) {
			headerReader.read(packet.getBuffer(), (int)packet.size());
		}

		if (headerReader.complete()) {

			client.setBufferSize(client.getMaxBufferSize());
			prepareRequestAndResponse(client);
			
			onHttpRequest(*request, *response);

			if (response->hasComplete()) {
				client.close();
            }
		}
	}

	void HttpConnection::onClientDisconnect(MultiConn & server, ClientSession & client) {
	}

	void HttpConnection::prepareRequestAndResponse(ClientSession & client) {
		OS::Socket * socket = client.getSocket();
		if (!request) {
			request = new HttpRequest(headerReader.getHeader(), *socket);
		}
		if (!response) {
			response = new HttpResponse(*socket);
		}
	}
	
	void HttpConnection::readContent(char * buffer, size_t size) {
		// content buffer manipulation
	}
	
	void HttpConnection::onHttpRequest(HttpRequest & request, HttpResponse & response) {
		if (getHandler()) {
			getHandler()->onHttpRequest(request, response);
		}
	}
	
	void HttpConnection::releaseRequest() {
		if (request) {
			delete request;
			request = NULL;
		}
	}
	
	void HttpConnection::releaseResponse() {
		if (response) {
			delete response;
			response = NULL;
		}
	}
	
}
