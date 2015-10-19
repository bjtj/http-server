#include <iostream>
#include <libhttp-server/Http.hpp>
#include <libhttp-server/HttpClient.hpp>

using namespace std;
using namespace HTTP;

class MyResponseHandler : public HttpResponseHandler {
private:
public:
    MyResponseHandler() {
	}
    virtual ~MyResponseHandler() {
	}

	virtual void onResponse(HttpClient & httpClient, HttpHeader & responseHeader, OS::Socket & socket) {
		int total = 0;
		char buffer[1024] = {0,};
		int len;
		int contentLength = responseHeader.getHeaderFieldIgnoreCaseAsInteger("Content-Length");
		while ((len = socket.recv(buffer, sizeof(buffer))) > 0) {
			total += len;
			if (total > contentLength) {
				len -= (total - contentLength);
			}
			string msg(buffer, len);
			cout << msg;
			if (total >= contentLength) {
				break;
			}
		}
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
	HttpClient client;
	MyResponseHandler handler;
	client.setHttpResponseHandler(&handler);
	client.request(url);
}
