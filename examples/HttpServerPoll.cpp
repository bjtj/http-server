#include <iostream>
#include <libhttp-server/HttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class Hello : public OnHttpRequestHandler {
public:
    Hello() {
	}
    virtual ~Hello() {
	}

	virtual void onRequest(HttpRequest & request, HttpResponse & response) {

		if (!request.remaining()) {
			string path = request.getPath();
			response.write("hello world - " + path);
			response.setComplete();
		}
	}
};

int main(int argc, char * args[]) {

    bool done = false;
	HttpServer server(8083);
    server.setUseThreadedMultiConnType(true);

	Hello hello;
	server.vpath("/hello/*", &hello);

	server.start();

	cout << "start" << endl;

	while (!done) {
		server.poll(1000);
	}

	cout << "stop" << endl;

	server.stop();

	return 0;
}