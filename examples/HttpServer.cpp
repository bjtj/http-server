#include <iostream>
#include "HttpServer.hpp"

using namespace std;
using namespace HTTP;


class Hello : public HttpRequestHandler
{
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

int main(int argc, char *args[]) {

	HttpServer server(8082);

	Hello hello;
	server.vpath("/hello", &hello);

	server.startAsync();

	getchar();

	server.stop();

	std::cout << "Done" << std::endl;
	
    return 0;
}
