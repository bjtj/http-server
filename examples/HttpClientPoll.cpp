#include <iostream>
#include <libhttp-server/HttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <libhttp-server/Url.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class MyHttpResponseHandler : public HttpResponseHandler<int> {
private:
public:
	MyHttpResponseHandler() {
	}
	virtual ~MyHttpResponseHandler() {
	}
	virtual void onResponse(HttpClient<int> & httpClient, HttpHeader & responseHeader, OS::Socket & socket, int userData) {
		string dump = HttpResponseDump::dump(responseHeader, socket);
		cout << dump << endl;
	}
};

class MyHttpClientPollListener : public HttpClientPollListener<int> {
private:
public:
    MyHttpClientPollListener() {}
    virtual ~MyHttpClientPollListener() {}
    virtual void onResponseHeader(const HttpHeader & responseHeader, int userData) {
        
    }
    virtual void onResponseDataChunk(const char * data, size_t len, int userData) {
        cout << string(data,len) << endl;
    }
};

int main(int argc, char * args[]) {

    bool done = false;
	HttpClient<int> client;
    
//	MyHttpResponseHandler handler;
//	client.setHttpResponseHandler(&handler);
    
    MyHttpClientPollListener pollListener;
    client.setHttpClientPollListener(&pollListener);

	client.requestStart(Url("http://www.example.com"), "GET", StringMap(), NULL, 0, 0);
    while (!done) {
        client.poll(1000);
    }

	getchar();

	return 0;
}