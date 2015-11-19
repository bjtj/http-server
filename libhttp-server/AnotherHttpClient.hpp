#ifndef __ANOTHER_HPP_CLIENT_HPP__
#define __ANOTHER_HPP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include "Connection.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "HttpHeaderReader.hpp"
#include "DataTransfer.hpp"
#include "Packet.hpp"
#include "Url.hpp"

namespace HTTP {

	/**
	 * @brief OnResponseHeaderListener
	 */
	class OnResponseHeaderListener {
	private:
	public:
		OnResponseHeaderListener();
		virtual ~OnResponseHeaderListener();
		virtual void onResponseHeader(HttpResponse & response) = 0;
	};

	/**
	 * @brief AnotherHttpClient
	 */
	class AnotherHttpClient {
	private:

		Url url;
		HttpRequest request;
		HttpResponse response;

		Connection * connection;
		OS::Socket * socket;
		OS::Selector selector;

		bool requestHeaderSent;
		bool responseHeaderReceived;
		bool readable;
		bool interrupted;
		bool complete;

		HttpHeaderReader headerReader;

	public:

		AnotherHttpClient(Url & url);
		virtual ~AnotherHttpClient();

		void setDataTransfer(DataTransfer * transfer);
		void connect();
		void execute();
		void interrupt();
		void clean();

		void sendRequestHeader();
		void sendRequestContent();
		void recvResponseHeader();
		void recvResponseContent();
	};

}

#endif