#include <iostream>
#include <libhttp-server/HttpClient.hpp>
#include <libhttp-server/ChunkedReader.hpp>

using namespace std;
using namespace HTTP;

class MyResponseHandler : public HttpResponseHandler<int> {
private:
public:
    MyResponseHandler() {
	}
    virtual ~MyResponseHandler() {
	}

	virtual void onResponse(HttpClient<int> & httpClient, HttpHeader & responseHeader, OS::Socket & socket, int userData) {

		string dump = HttpResponseDump::dump(responseHeader, socket);

		cout << dump << endl;
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
	MyResponseHandler handler;
	client.setHttpResponseHandler(&handler);
	client.setFollowRedirect(true);
	client.request(url, 0);

	getchar();
}
