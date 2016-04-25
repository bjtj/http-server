#ifndef __ANOTHER_HPP_CLIENT_HPP__
#define __ANOTHER_HPP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include "Connection.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "HttpHeaderReader.hpp"
#include "DataTransfer.hpp"
#include "ChunkedTransfer.hpp"
#include "Packet.hpp"
#include "Url.hpp"

namespace HTTP {

	/**
	 * @brief UserData
	 */
	class UserData {
    private:
    public:
        UserData() {}
        virtual ~UserData() {}
    };

	/**
	 * @brief OnResponseHeaderListener
	 */
	class OnResponseListener {
	private:
	public:
		OnResponseListener();
		virtual ~OnResponseListener();
		virtual UTIL::AutoRef<DataSink> getDataSink() = 0;
		virtual DataTransfer * createDataTransfer(HttpHeader & header, UTIL::AutoRef<DataSink> sink);
		virtual void onResponseHeader(HttpResponse & response, UTIL::AutoRef<UserData> userData);
        virtual void onTransferDone(HttpResponse & response, UTIL::AutoRef<DataSink> sink, UTIL::AutoRef<UserData> userData) = 0;
        virtual void onError(OS::Exception & e, UTIL::AutoRef<UserData> userData) = 0;
	};

	/**
	 * @brief AnotherHttpClient
	 */
	class AnotherHttpClient {
	private:
		bool debug;
		Url url;
		HttpRequest request;
		HttpResponse response;
		Connection * connection;
		UTIL::AutoRef<OS::Socket> socket;
		OS::Selector selector;
		bool requestHeaderSent;
		bool responseHeaderReceived;
		bool readable;
		bool interrupted;
		bool complete;
		HttpHeaderReader responseHeaderReader;
        OnResponseListener * responseListener;
		unsigned long connectionTimeout;
		unsigned long recvTimeout;
        bool followRedirect;
		UTIL::AutoRef<UserData> userData;

	public:

        AnotherHttpClient();
		AnotherHttpClient(const Url & url);
		virtual ~AnotherHttpClient();

		void logd(const std::string & msg);
		void setDebug(bool debug);
        void reconnect();
		void connect();
        void closeConnection();
        void setUrl(const Url & url);
		void setRequest(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields);
        void setRequestWithFixedTransfer(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, size_t size);
		void setRequestWithChunkedTransfer(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer);
        void setFixedTransfer(UTIL::AutoRef<DataTransfer> transfer, size_t size);
		void setChunkedTransfer(UTIL::AutoRef<DataTransfer> transfer);
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
		void setConnectionTimeout(unsigned long timeout);
		void setRecvTimeout(unsigned long timeout);
        bool needRedirect();
        void handleRedirect();
        void setFollowRedirect(bool followRedirect);
        void setOnResponseListener(OnResponseListener * responseListener);
		void setUserData(UTIL::AutoRef<UserData> userData);
        Url & getUrl();
        HttpResponse & getResponse();
        
	};

}

#endif
