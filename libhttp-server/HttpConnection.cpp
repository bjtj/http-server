#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

#include "HttpConnection.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
    using namespace OS;
	using namespace UTIL;

	const static Logger & logger = LoggerFactory::getDefaultLogger();
	
	/**
	 * @brief http connection constructor
	 */
    HttpConnection::HttpConnection(OnHttpRequestHandler * requestHandler)
		: requestHandler(requestHandler), request(NULL), response(NULL) {
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
			headerReader.read(packet.getBuffer(), packet.length());
			packet.clear();
		}

		if (headerReader.complete()) {

			client.setBufferSize(client.getMaxBufferSize());
			prepareRequestAndResponse(client);
			
			request->setContentPacket(packet);
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
	
	void HttpConnection::onHttpRequest(HttpRequest & request, HttpResponse & response) {
		if (requestHandler) {
			requestHandler->onHttpRequest(request, response);
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
