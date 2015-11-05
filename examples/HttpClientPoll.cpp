#include <iostream>
#include <libhttp-server/HttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
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

int main(int argc, char * args[]) {

	HttpClient<int> client;
	MyHttpResponseHandler handler;
	client.setHttpResponseHandler(&handler);

	client.request(Url("http://www.example.com"), 0);

	getchar();

	return 0;
}