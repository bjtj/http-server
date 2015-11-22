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
	class OnResponseListener {
	private:
	public:
		OnResponseListener();
		virtual ~OnResponseListener();
		virtual void onResponseHeader(HttpResponse & response) = 0;
        virtual void onTransferDone(DataTransfer * transfer) = 0;
        virtual void onError() = 0;
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

		HttpHeaderReader responseHeaderReader;
        
        OnResponseListener * responseListener;
        
        bool followRedirect;

	public:

        AnotherHttpClient();
		AnotherHttpClient(const Url & url);
		virtual ~AnotherHttpClient();

        void reconnect();
		void connect();
        void closeConnection();
        void setUrl(const Url & url);
        void setRequest(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer);
        void setDataTransfer(UTIL::AutoRef<DataTransfer> transfer);
		void execute();
        void communicate();
		void interrupt();
		void clear();
        void clearStates();
        
		void sendRequestHeader();
		void sendRequestContent();
		void recvResponseHeader();
		void recvResponseContent();
        
        void onResponseTransferDone();
        void setComplete();
        
        bool needRedirect();
        void handleRedirect();
        void setFollowRedirect(bool followRedirect);
                
        void setOnResponseListener(OnResponseListener * responseListener);
        
        Url & getUrl();
        HttpResponse & getResponse();
        
	};

}

#endif