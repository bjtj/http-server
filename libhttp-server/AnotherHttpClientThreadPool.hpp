#ifndef __ANOTHER_HTTP_CLIENT_THREAD_POOL_HPP__
#define __ANOTHER_HTTP_CLIENT_THREAD_POOL_HPP__

#include <string>

#include <liboslayer/AutoRef.hpp>
#include <liboslayer/ThreadPool.hpp>
#include "AnotherHttpClient.hpp"
#include "Url.hpp"

namespace HTTP {
    
    
    class UserData {
    private:
    public:
        UserData() {}
        virtual ~UserData() {}
    };
    
    /**
     * @brief
     */
    class OnRequestCompleteListener {
    private:
    public:
        OnRequestCompleteListener() {}
        virtual ~OnRequestCompleteListener() {}
        
        virtual void onRequestComplete(Url & url, HttpResponse & response, const std::string & content, UserData * userData) = 0;
        virtual void onRequestError(Url & url, UserData * userData) = 0;
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
        void setUserData(UTIL::AutoRef<UserData> userData);
        void setRequest(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer);
        virtual void run();
        
        virtual void onResponseHeader(HttpResponse & response);
        virtual void onTransferDone(DataTransfer * transfer);
        virtual void onError();
        
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
        
        void setRequest(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData);
        void setRequest(const Url & url, const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData);
        
        void setOnRequestCompleteListener(OnRequestCompleteListener * listener);
    };
}

#endif
