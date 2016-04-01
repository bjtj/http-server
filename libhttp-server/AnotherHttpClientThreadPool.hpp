#ifndef __ANOTHER_HTTP_CLIENT_THREAD_POOL_HPP__
#define __ANOTHER_HTTP_CLIENT_THREAD_POOL_HPP__

#include <string>

#include <liboslayer/AutoRef.hpp>
#include <liboslayer/ThreadPool.hpp>
#include "AnotherHttpClient.hpp"
#include "Url.hpp"

namespace HTTP {
    
    /**
     * @brief
     */
    class OnRequestCompleteListener {
    private:
    public:
        OnRequestCompleteListener() {}
        virtual ~OnRequestCompleteListener() {}
        
        virtual void onRequestComplete(Url & url, HttpResponse & response, const std::string & content, UserData * userData) = 0;
        virtual void onRequestError(OS::Exception & e, Url & url, UserData * userData) = 0;
    };
    
    
    /**
     * @brief
     */
    class AnotherHttpClientThread : public UTIL::FlaggableThread, public OnResponseListener {
    private:
        UTIL::AutoRef<UserData> userData;
        AnotherHttpClient httpClient;
        OnRequestCompleteListener * listener;
        
    public:
        AnotherHttpClientThread();
        virtual ~AnotherHttpClientThread();
		UTIL::AutoRef<DataSink> getDataSink();
        void setUserData(UTIL::AutoRef<UserData> userData);
        void setRequestWithFixedTransfer(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, size_t size);
		void setRequestWithChunkedTransfer(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer);
        virtual void run();
        virtual void onTransferDone(HttpResponse & response, UTIL::AutoRef<DataSink> sink, UTIL::AutoRef<UserData> userData);
        virtual void onError(OS::Exception & e, UTIL::AutoRef<UserData> userData);
        
        void setOnRequestCompleteListener(OnRequestCompleteListener * listener);
        
    };
    
    
    /**
     * @brief
     */
    class AnotherHttpClientThreadPool : public UTIL::ThreadPool {
    private:
        OnRequestCompleteListener * listener;
    public:
        AnotherHttpClientThreadPool(size_t maxThreads);
        virtual ~AnotherHttpClientThreadPool();
        
        void setRequestWithFixedTransfer(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, size_t size, UTIL::AutoRef<UserData> userData);
		void setRequestWithChunkedTransfer(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData);
		void setRequestWithFixedTransfer(const Url & url, const std::string & method, const std::map<std::string, std::string> & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, size_t size, UTIL::AutoRef<UserData> userData);
		void setRequestWithChunkedTransfer(const Url & url, const std::string & method, const std::map<std::string, std::string> & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData);
        void setRequestWithFixedTransfer(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, size_t size, UTIL::AutoRef<UserData> userData);

		void setRequestWithChunkedTransfer(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData);
        
        void setOnRequestCompleteListener(OnRequestCompleteListener * listener);
    };
}

#endif
