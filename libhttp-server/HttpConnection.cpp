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
    HttpConnection::HttpConnection(OnHttpRequestHandler * handler)
		: OnHttpRequestHandlerDecorator(handler), request(NULL), response(NULL) {
	}
	HttpConnection::~HttpConnection() {
		releaseRequest();
		releaseResponse();
	}

	void HttpConnection::onClientConnect(MultiConn & server, ClientSession & client) {
		client.setBufferSize(1);
		logger.logv("connected/client");
	}

	void HttpConnection::onClientReceive(MultiConn & server, ClientSession & client, Packet & packet) {

		if (!headerReader.complete()) {
			headerReader.read(packet.getBuffer(), packet.length());
			packet.clear();
		}

		if (headerReader.complete()) {

			logger.logv("ready");

			client.setBufferSize(client.getMaxBufferSize());
			prepareRequestAndResponse(client);
			
			request->setContentPacket(packet);
			onHttpRequest(*request, *response);

			if (response->hasComplete()) {
				logger.logv("complete");
				client.close();
            } else {
				logger.logv("what?");
			}
		}
	}

	void HttpConnection::onClientDisconnect(MultiConn & server, ClientSession & client) {
		logger.logv("disconnected/client");
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
