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
    template <typename T>
    class HttpClientRequest {
    private:
        Url url;
        std::string method;
        HttpHeader header;
        char * data;
        int len;
        T userData;
        
    public:
        HttpClientRequest();
        HttpClientRequest(Url & url, std::string & method, char * data, int len, T userData);
        virtual ~HttpClientRequest();
        
        Url & getUrl();
        HttpHeader & getHeader();
        std::string & getMethod();
        char * getData();
        int getDataLength();
        void setData(char * data, int len);
        T getUserData();
    };
    
    /**
     * @brief http client thread
     */
    template <typename T>
    class HttpClientThread : public OS::Thread {
    private:
        HttpClient<T> client;
        std::queue<HttpClientRequest<T>> * requestQueue;
        OS::Semaphore * sem;
        
    public:
        HttpClientThread(std::queue<HttpClientRequest<T>> * requestQueue, OS::Semaphore * sem);
        virtual ~HttpClientThread();
        virtual void run();
        HttpClient<T> & getHttpClient();
    };

    /**
     * @brief http client thread pool
     */
    template <typename T>
	class HttpClientThreadPool {
	private:
		int maxThread;
        OS::Semaphore sem;
        std::queue<HttpClientRequest<T>> requestQueue;
        HttpResponseHandler<T> * handler;
        std::vector<HttpClientThread<T>> pool;
        
	public:
		HttpClientThreadPool(int maxThread);
		virtual ~HttpClientThreadPool();
        void setHttpResponseHandler(HttpResponseHandler<T> * handler);
        void setFollowRedirect(bool followRedirect);
        void request(Url & url, std::string & method, char * data, int len, T userData);
        void start();
        void stop();
	};
    
    
    template <typename T>
    HttpClientRequest<T>::HttpClientRequest() : data(NULL), len(0) {
    }
    template <typename T>
    HttpClientRequest<T>::HttpClientRequest(Url & url, std::string & method, char * data, int len, T userData)
    : url(url), method(method), data(data), len(len), userData(userData) {
    }
    template <typename T>
    HttpClientRequest<T>::~HttpClientRequest() {
    }
    template <typename T>
    Url & HttpClientRequest<T>::getUrl() {
        return url;
    }
    template <typename T>
    HttpHeader & HttpClientRequest<T>::getHeader() {
        return header;
    }
    template <typename T>
    std::string & HttpClientRequest<T>::getMethod() {
        return method;
    }
    template <typename T>
    char * HttpClientRequest<T>::getData() {
        return data;
    }
    template <typename T>
    int HttpClientRequest<T>::getDataLength() {
        return len;
    }
    template <typename T>
    void HttpClientRequest<T>::setData(char * data, int len) {
        this->data = data;
        this->len = len;
    }
    template <typename T>
    T HttpClientRequest<T>::getUserData() {
        return userData;
    }
    
    
    template <typename T>
    HttpClientThread<T>::HttpClientThread(std::queue<HttpClientRequest<T>> * requestQueue, OS::Semaphore * sem)
    : requestQueue(requestQueue), sem(sem) {
    }
    
    template <typename T>
    HttpClientThread<T>::~HttpClientThread() {
    }
    
    template <typename T>
    void HttpClientThread<T>::run() {
        
        while (!interrupted()) {
            bool empty = false;
            HttpClientRequest<T> req;
            sem->wait();
            empty = requestQueue->empty();
            if (!empty) {
                req = requestQueue->front();
                requestQueue->pop();
            }
            sem->post();
            
            if (!empty) {
                client.request(req.getUrl(), req.getMethod(), req.getData(), req.getDataLength(), req.getUserData());
            } else {
                OS::idle(10);
            }
        }
    }
    
    template <typename T>
    HttpClient<T> & HttpClientThread<T>::getHttpClient() {
        return client;
    }
    
    template <typename T>
    HttpClientThreadPool<T>::HttpClientThreadPool(int maxThread) : maxThread(maxThread), sem(1), handler(NULL) {
        for (int i = 0; i < maxThread; i++) {
            pool.push_back(HttpClientThread<T>(&requestQueue, &sem));
        }
    }
    template <typename T>
    HttpClientThreadPool<T>::~HttpClientThreadPool() {
        
    }
    template <typename T>
    void HttpClientThreadPool<T>::setHttpResponseHandler(HttpResponseHandler<T> * handler) {
        this->handler = handler;
        
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].getHttpClient().setHttpResponseHandler(handler);
        }
    }
    template <typename T>
    void HttpClientThreadPool<T>::setFollowRedirect(bool followRedirect) {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].getHttpClient().setFollowRedirect(followRedirect);
        }
    }
    template <typename T>
    void HttpClientThreadPool<T>::request(Url & url, std::string & method, char * data, int len, T userData) {
        sem.wait();
        requestQueue.push(HttpClientRequest<T>(url, method, data, len, userData));
        sem.post();
    }
    template <typename T>
    void HttpClientThreadPool<T>::start() {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].start();
        }
    }
    template <typename T>
    void HttpClientThreadPool<T>::stop() {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].interrupt();
            pool[i].join();
        }
    }
}

#endif
