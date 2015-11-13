#include <iostream>
#include <libhttp-server/HttpClient.hpp>
#include <libhttp-server/ChunkedReader.hpp>

using namespace std;
using namespace HTTP;

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
    virtual void onComplete(HttpClient<int> & httpClient, int userData) {
        cout << "done" << endl;
    }
    virtual void onError(HttpClient<int> & httpClient, int userData) {
        cout << "error" << endl;
    }
};


void test_url();
void print_url(Url & url);
void test_request(Url & url);

int main(int argc, char *args[]) {

	Url url("http://www.google.com");

	// test_url();
	test_request(url);
	
    return 0;
}

void test_url() {
	Url url("http://example.com");
	print_url(url);

	url = "http://wow.com:8080";
	print_url(url);

	url = "http://example.com:8080";
	print_url(url);

	url = "http://wow.com:8080/path";
	print_url(url);
}

void print_url(Url & url) {
	string protocol = url.getProtocol();
	string host = url.getHost();
	string port = url.getPort();
	string path = url.getPath();
	cout << "Protoco: " << protocol <<
		", Host: " << host <<
		", Port: " << port <<
		", Path: " << path << endl;
}

void test_request(Url & url) {
	HttpClient<int> client;
    MyHttpClientPollListener pollListener;
    client.setHttpClientPollListener(&pollListener);
	client.setFollowRedirect(true);
	client.request(url, 0);

	getchar();
}
