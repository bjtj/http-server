#include "AnotherHttpClient.hpp"

#include <liboslayer/Logger.hpp>
#include <string>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();

	/**
	 * @brief OnResponseHeaderListener
	 */
	OnResponseHeaderListener::OnResponseHeaderListener() {
	}

	OnResponseHeaderListener::~OnResponseHeaderListener() {
	}

	/**
	 * @brief AnotherHttpClient
	 */
	AnotherHttpClient::AnotherHttpClient(Url & url) : url(url), connection(NULL), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false) {
	}
	AnotherHttpClient::~AnotherHttpClient() {
	}

	void AnotherHttpClient::setDataTransfer(DataTransfer * transfer) {
	}
	void AnotherHttpClient::connect() {

		if (!connection) {
			string remoteHost = url.getHost();
			int remotePort = url.getIntegerPort();
			socket = new Socket(remoteHost.c_str(), remotePort);

			connection = new Connection(*socket);
		}

		socket->registerSelector(selector);
		socket->connect();
	}
	void AnotherHttpClient::execute() {

		clean();

		try {

			while (!interrupted) {

				if (selector.select(100)) {

					if (selector.isWriteableSelected(*socket)) {
						sendRequestHeader();
						sendRequestContent();
					}

					if (selector.isReadableSelected(*socket)) {
						recvResponseHeader();
						recvResponseContent();
					}

					if (complete) {
						break;
					}
				}
			}

		} catch (IOException e) {
			logger.loge("IOException " + e.getMessage());
		}

		clean();
	}

	void AnotherHttpClient::interrupt() {
		interrupted = true;
	}

	void AnotherHttpClient::clean() {

		if (connection) {

			delete connection;
			connection = NULL;

			socket->unregisterSelector(selector);
			socket->close();
			delete socket;
			socket = NULL;
		}

		requestHeaderSent = false;
		responseHeaderReceived = false;
		readable = false;
		interrupted = false;
		complete = false;
	}

	void AnotherHttpClient::sendRequestHeader() {

		if (!requestHeaderSent) {
			string header = request.getHeader().toString();
			connection->send(header.c_str(), header.length());
			requestHeaderSent = true;
		}
	}
	void AnotherHttpClient::sendRequestContent() {

		if (!requestHeaderSent) {
			return;
		}

		DataTransfer * transfer = request.getTransfer();
		if (!transfer) {
			readable = true;
			return;
		}

		transfer->send(*connection);

		if (transfer->isCompleted()) {
			readable = true;
		}
	}
	void AnotherHttpClient::recvResponseHeader() {

		if (readable && !responseHeaderReceived) {

			char buf;
			connection->recv(&buf, 1);

			headerReader.read(&buf, 1);

			if (headerReader.complete()) {
				responseHeaderReceived = true;
			}
		}
	}
	void AnotherHttpClient::recvResponseContent() {

		if (readable && responseHeaderReceived) {
			DataTransfer * transfer = response.getTransfer();
			if (!transfer) {
				return;
			}

			Packet & packet = connection->read();
			transfer->recv(packet);

			if (transfer->isCompleted()) {
				complete = true;
			}
		}
	}

}