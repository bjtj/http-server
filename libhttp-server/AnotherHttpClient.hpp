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

	class SocketMaker {
	public:
		SocketMaker() {}
		virtual ~SocketMaker() {}
		virtual OS::AutoRef<OS::Socket> make(const std::string & protocol, const OS::InetAddress & addr) = 0;
	};


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
	class OnHttpResponseListener {
	private:
	public:
		OnHttpResponseListener();
		virtual ~OnHttpResponseListener();
		virtual OS::AutoRef<DataSink> getDataSink();
		virtual DataTransfer * createDataTransfer(HttpHeader & header, OS::AutoRef<DataSink> sink);
		virtual void onResponseHeader(HttpResponse & response, OS::AutoRef<UserData> userData);
        virtual void onTransferDone(HttpResponse & response, OS::AutoRef<DataSink> sink, OS::AutoRef<UserData> userData) = 0;
        virtual void onError(OS::Exception & e, OS::AutoRef<UserData> userData) = 0;
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
        OS::AutoRef<Connection> connection;
		OS::AutoRef<SocketMaker> socketMaker;
		OS::AutoRef<OS::Socket> socket;
		OS::Selector selector;
		bool requestHeaderSent;
		bool responseHeaderReceived;
		bool readable;
		bool interrupted;
		bool complete;
		HttpHeaderReader responseHeaderReader;
        OnHttpResponseListener * responseListener;
		unsigned long connectionTimeout;
		unsigned long recvTimeout;
        bool followRedirect;
		OS::AutoRef<UserData> userData;

	public:

        AnotherHttpClient();
		AnotherHttpClient(OS::AutoRef<SocketMaker> socketMaker);
		AnotherHttpClient(const Url & url);
		virtual ~AnotherHttpClient();

		void logd(const std::string & msg);
		void setDebug(bool debug);
        void reconnect();
		void connect();
        void close();
        void setUrl(const Url & url);
		void setRequest(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields);
        void setRequestWithFixedTransfer(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, OS::AutoRef<DataTransfer> transfer, size_t size);
		void setRequestWithChunkedTransfer(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, OS::AutoRef<DataTransfer> transfer);
        void setFixedTransfer(OS::AutoRef<DataTransfer> transfer, size_t size);
		void setChunkedTransfer(OS::AutoRef<DataTransfer> transfer);
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
        void setOnHttpResponseListener(OnHttpResponseListener * responseListener);
		void setUserData(OS::AutoRef<UserData> userData);
        Url getUrl();
        HttpResponse & getResponse();
        
	};

}

#endif
