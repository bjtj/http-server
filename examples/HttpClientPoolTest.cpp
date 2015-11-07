#include <iostream>
#include <libhttp-server/HttpClientThreadPool.hpp>
#include <libhttp-server/Url.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class MyHttpClientPollListener : public HttpClientPollListener<int> {
private:
public:
    MyHttpClientPollListener() {}
    virtual ~MyHttpClientPollListener() {}
    virtual void onRequestHeader(HttpClient<int> & httpClient, const HttpHeader & requestHeader, int userData) {
//        cout << requestHeader.toString() << endl;
    }
    virtual void onResponseHeader(HttpClient<int> & httpClient, const HttpHeader & responseHeader, int userData) {
//        cout << responseHeader.toString() << endl;
    }
    virtual void onResponseDataChunk(HttpClient<int> & httpClient, const HttpHeader & responseHeader, const char * data, size_t len, int userData) {
//        cout << string(data,len) << endl;
        int statusCode = httpClient.getStatusCode();
        if (statusCode != 301 && statusCode != 302) {
            httpClient.getStringBuffer().append(data, len);
        }
    }
    virtual void onComplete(HttpClient<int> & httpClient) {
        cout << httpClient.getStringBuffer() << endl;
        cout << "done" << endl;
    }
    virtual void onError(HttpClient<int> & httpClient) {
        cout << "error" << endl;
    }
};

class MyHttpResponseHandler : public HttpResponseHandler<int> {
private:
public:
    virtual void onHttpResponse(HttpClient<int> & httpClient, const HttpHeader & responseHeader, const string & content) {
        cout << content << endl;
    }
    
    virtual void onError(HttpClient<int> & httpClient) {
        cout << "error" << endl;
    }
};

int main(int argc, char * args[]) {
    
    HttpClientThreadPool<int> clientPool(10);
    
    clientPool.setFollowRedirect(true);
    
//    MyHttpClientPollListener pollListener;
//    clientPool.setHttpClientPollListener(&pollListener);
    
    MyHttpResponseHandler handler;
    clientPool.setHttpClientPollListener(&handler);
    
    clientPool.start();
    
//    clientPool.request(Url("http://www.example.com"), "GET", StringMap(), NULL, 0, 0);
//    clientPool.request(Url("http://www.example.com"), "GET", StringMap(), NULL, 0, 0);
//    clientPool.request(Url("http://www.example.com"), "GET", StringMap(), NULL, 0, 0);
    
    clientPool.request(Url("http://www.gogole.com"), "GET", StringMap(), NULL, 0, 0);
    clientPool.request(Url("http://www.blogger.com"), "GET", StringMap(), NULL, 0, 0);
    clientPool.request(Url("http://www.gmail.com"), "GET", StringMap(), NULL, 0, 0);
    
    getchar();
    
    clientPool.stop();
    
    return 0;
}