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

namespace http {

	class SocketMaker {
	public:
		SocketMaker() {}
		virtual ~SocketMaker() {}
		virtual osl::AutoRef<osl::Socket> make(const std::string & protocol, const osl::InetAddress & addr) = 0;
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
		virtual osl::AutoRef<DataSink> getDataSink();
		virtual DataTransfer * createDataTransfer(HttpHeader & header, osl::AutoRef<DataSink> sink);
		virtual void onResponseHeader(HttpResponse & response, osl::AutoRef<UserData> userData);
        virtual void onTransferDone(HttpResponse & response, osl::AutoRef<DataSink> sink, osl::AutoRef<UserData> userData) = 0;
        virtual void onError(osl::Exception & e, osl::AutoRef<UserData> userData) = 0;
	};

	/**
	 * @brief AnotherHttpClient
	 */
	class AnotherHttpClient {
	private:
		bool _debug;
		Url url;
		HttpRequest request;
		HttpResponse response;
        osl::AutoRef<Connection> connection;
		osl::AutoRef<SocketMaker> socketMaker;
		osl::AutoRef<osl::Socket> socket;
		osl::Selector selector;
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
		osl::AutoRef<UserData> userData;

	public:

        AnotherHttpClient();
		AnotherHttpClient(osl::AutoRef<SocketMaker> socketMaker);
		AnotherHttpClient(const Url & url);
		virtual ~AnotherHttpClient();

		void logd(const std::string & msg);
		void setDebug(bool debug);
        void reconnect();
		void connect();
        void close();
        void setUrl(const Url & url);
		void setRequest(const std::string & method, const osl::LinkedStringMap & additionalHeaderFields);
        void setRequestWithFixedTransfer(const std::string & method, const osl::LinkedStringMap & additionalHeaderFields, osl::AutoRef<DataTransfer> transfer, size_t size);
		void setRequestWithChunkedTransfer(const std::string & method, const osl::LinkedStringMap & additionalHeaderFields, osl::AutoRef<DataTransfer> transfer);
        void setFixedTransfer(osl::AutoRef<DataTransfer> transfer, size_t size);
		void setChunkedTransfer(osl::AutoRef<DataTransfer> transfer);
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
		void setUserData(osl::AutoRef<UserData> userData);
        Url getUrl();
        HttpResponse & getResponse();
        
	};

}

#endif
