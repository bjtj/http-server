#include "HttpClientThreadPool.hpp"

namespace HTTP {
    
    using namespace std;
    using namespace OS;
	
    HttpClientRequest::HttpClientRequest() : data(NULL), len(0) {
    }
    
    HttpClientRequest::HttpClientRequest(Url & url, string & method, char * data, int len) :
        url(url), method(method), data(data), len(len) {
	}
	HttpClientRequest::~HttpClientRequest() {
	}

	Url & HttpClientRequest::getUrl() {
		return url;
	}
	
	HttpHeader & HttpClientRequest::getHeader() {
		return header;
	}
    
    string & HttpClientRequest::getMethod() {
        return method;
    }
	
	char * HttpClientRequest::getData() {
		return data;
	}
	
	int HttpClientRequest::getDataLength() {
		return len;
	}
	
	void HttpClientRequest::setData(char * data, int len) {
		this->data = data;
		this->len = len;
	}
    
    
    
    HttpClientThread::HttpClientThread(std::queue<HttpClientRequest> & requestQueue, Semaphore & sem) : requestQueue(requestQueue), sem(sem) {
        
    }
    
    HttpClientThread::~HttpClientThread() {
        
    }
    
    void HttpClientThread::run() {
        
        while (!interrupted()) {
            bool empty = false;
            HttpClientRequest req;
            sem.wait();
            empty = requestQueue.empty();
            if (!empty) {
                req = requestQueue.front();
                requestQueue.pop();
            }
            sem.post();
            
            if (!empty) {
                client.request(req.getUrl(), req.getMethod(), req.getData(), req.getDataLength());
            } else {
                idle(10);
            }
        }
    }
    
    HttpClient & HttpClientThread::getHttpClient() {
        return client;
    }
    
    
    HttpClientThreadPool::HttpClientThreadPool(int maxThread) : maxThread(maxThread), sem(1), handler(NULL) {
        for (int i = 0; i < maxThread; i++) {
            pool.push_back(HttpClientThread(requestQueue, sem));
        }
    }
    
    HttpClientThreadPool::~HttpClientThreadPool() {
        
    }
    
    void HttpClientThreadPool::setHttpResponseHandler(HttpResponseHandler * handler) {
        this->handler = handler;
        
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].getHttpClient().setHttpResponseHandler(handler);
        }
    }
    
    void HttpClientThreadPool::setFollowRedirect(bool followRedirect) {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].getHttpClient().setFollowRedirect(followRedirect);
        }
    }
    
    void HttpClientThreadPool::request(Url & url, string & method, char * data, int len) {
        sem.wait();
        requestQueue.push(HttpClientRequest(url, method, data, len));
        sem.post();
    }
    
    void HttpClientThreadPool::start() {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].start();
        }
    }
	
    void HttpClientThreadPool::stop() {
        for (size_t i = 0; i < pool.size(); i++) {
            pool[i].interrupt();
            pool[i].join();
        }
    }
}
