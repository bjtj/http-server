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

class MyHttpClientPollListener : public HttpClientPollListener<int> {
private:
public:
    MyHttpClientPollListener() {}
    virtual ~MyHttpClientPollListener() {}
    virtual void onRequestHeader(HttpClient<int> & httpClient, const HttpHeader & requestHeader, int userData) {
        cout << requestHeader.toString() << endl;
    }
    virtual void onResponseHeader(HttpClient<int> & httpClient, const HttpHeader & responseHeader, int userData) {
		cout << responseHeader.toString() << endl;
    }
    virtual void onResponseDataChunk(HttpClient<int> & httpClient, const HttpHeader & responseHeader, const char * data, size_t len, int userData) {
        cout << string(data,len) << endl;
    }
	virtual void onComplete(HttpClient<int> & httpClient) {
		cout << "done" << endl;
	}
	virtual void onError(HttpClient<int> & httpClient) {
		cout << "error" << endl;
	}
};

int main(int argc, char * args[]) {

    bool done = false;
	HttpClient<int> client;
    
    client.setFollowRedirect(true);
    
    MyHttpClientPollListener pollListener;
    client.setHttpClientPollListener(&pollListener);

	// client.requestStart(Url("http://www.example.com"), "GET", StringMap(), NULL, 0, 0);
	//client.requestStart(Url("http://192.168.0.2:54030/"), "GET", StringMap(), NULL, 0, 0);
	//client.requestStart(Url("http://192.168.0.2:51001/"), "GET", StringMap(), NULL, 0, 0);
	client.requestStart(Url("http://www.google.com/"), "GET", StringMap(), NULL, 0, 0);
	
    while (!done) {
        
        if (client.getStatus() == HttpRequestStatus::IDLE_STATUS) {
            char buffer[4096] = {0,};
            fgets(buffer, sizeof(buffer), stdin);
            buffer[strlen(buffer) - 1] = 0;
            client.requestStart(Url(buffer), "GET", StringMap(), NULL, 0, 0);
        }
        client.poll(1000);
    }

	getchar();

	return 0;
}