#include <liboslayer/Text.hpp>

#include "HttpConnection.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	
	/**
	 * @brief http connection constructor
	 */
	HttpConnection::HttpConnection(HttpRequestHandler * handler)
		: HttpRequestHandlerDecorator(handler), request(NULL), response(NULL) {
	}
	HttpConnection::~HttpConnection() {
		releaseRequest();
		releaseResponse();
	}

	void HttpConnection::onConnect(MultiConn & server, OS::Socket & client) {
	}

	void HttpConnection::onReceive(MultiConn & server, OS::Socket & client, Packet & packet) {

		if (!headerReader.complete()) {
			int offset = headerReader.read(packet.getBuffer(), packet.size());
			readContent(packet.getBuffer() + offset, packet.length() - offset);
		} else {
			readContent(packet.getBuffer(), packet.length());
		}

		if (headerReader.complete()) {
			
			if (!request) {
				request = new HttpRequest(headerReader.getHeader(), client);
			}
			
			if (!response) {
				response = new HttpResponse(client);
			}
			
			onRequest(*request, *response);

			if (response->hasComplete()) {
				server.disconnect(client);
			}
		}
	}

	void HttpConnection::onDisconnect(MultiConn & server, OS::Socket & client) {
	}
	void HttpConnection::readContent(char * buffer, size_t size) {
		// content buffer manipulation
	}
	void HttpConnection::onRequest(HttpRequest & request, HttpResponse & response) {
		if (getHandler()) {
			getHandler()->onRequest(request, response);
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
