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

	void HttpConnection::onClientConnect(MultiConn & server, Connection & connection) {
		connection.setBufferSize(1);
	}

	void HttpConnection::onClientReceive(MultiConn & server, Connection & connection, Packet & packet) {

		if (!headerReader.complete()) {
			headerReader.read(packet.getData(), (int)packet.getLength());
			packet.clear();
		}

		if (headerReader.complete()) {

			connection.setBufferSize(connection.getMaxBufferSize());
			prepareRequestAndResponse(connection);
			
			request->setContentPacket(&packet);
			onHttpRequest(*request, *response);
		}
	}
    
    void HttpConnection::onClientWriteable(MultiConn & server, Connection & connection) {
        
        if (response && response->hasComplete()) {
            if (!response->completeContentTransfer()) {
                response->sendContent();
            }
            
            if (response->completeContentTransfer()) {
                connection.close();
            }
        }
    }

	void HttpConnection::onClientDisconnect(MultiConn & server, Connection & connection) {
	}

	void HttpConnection::prepareRequestAndResponse(Connection & connection) {
		OS::Socket * socket = connection.getSocket();
		if (!request) {
			request = new HttpRequest(headerReader.getHeader());
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
