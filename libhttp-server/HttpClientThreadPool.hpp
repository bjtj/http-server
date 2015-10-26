#ifndef __HTTP_CLIENT_POOL_THREAD_HPP__
#define __HTTP_CLIENT_POOL_THREAD_HPP__

#include <string>
#include <vector>
#include <queue>
#include <liboslayer/os.hpp>
#include "HttpHeader.hpp"
#include "HttpClient.hpp"
#include "Url.hpp"

namespace HTTP {
    
    /**
     * @brief http client request
     */
    class HttpClientRequest {
    private:
        Url url;
        std::string method;
        HttpHeader header;
        char * data;
        int len;
        
    public:
        HttpClientRequest();
        HttpClientRequest(Url & url, std::string & method, char * data, int len);
        virtual ~HttpClientRequest();
        
        Url & getUrl();
        HttpHeader & getHeader();
        std::string & getMethod();
        char * getData();
        int getDataLength();
        void setData(char * data, int len);
    };
    
    /**
     * @brief http client thread
     */
    class HttpClientThread : public OS::Thread {
    private:
        HttpClient client;
        std::queue<HttpClientRequest> * requestQueue;
        OS::Semaphore * sem;
        
    public:
        HttpClientThread(std::queue<HttpClientRequest> * requestQueue, OS::Semaphore * sem);
        virtual ~HttpClientThread();
        virtual void run();
        HttpClient & getHttpClient();
    };

    /**
     * @brief http client thread pool
     */
	class HttpClientThreadPool {
	private:
		int maxThread;
        OS::Semaphore sem;
        std::queue<HttpClientRequest> requestQueue;
        HttpResponseHandler * handler;
        std::vector<HttpClientThread> pool;
        
	public:
		HttpClientThreadPool(int maxThread);
		virtual ~HttpClientThreadPool();
        void setHttpResponseHandler(HttpResponseHandler * handler);
        void setFollowRedirect(bool followRedirect);
        void request(Url & url, std::string & method, char * data, int len);
        void start();
        void stop();
	};

}

#endif
