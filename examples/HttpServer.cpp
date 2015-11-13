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

	virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) {

		ChunkedBuffer & buffer = request.getChunkedBuffer();
		request.readChunkedBuffer(buffer);

		if (request.completeContentRead()) {
			string path = request.getPath();
			response.write("hello world - " + path);
			response.setComplete();
		}
	}
};


class Late : public OnHttpRequestHandler {
public:
    Late() {
	}
    virtual ~Late() {
	}

	virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) {

		ChunkedBuffer & buffer = request.getChunkedBuffer();
		request.readChunkedBuffer(buffer);

		if (request.completeContentRead()) {

			string timeout = request.getParameter("timeout");
			long t = Text::toInt(timeout);
			idle(t);
			response.write("LATE - " + timeout);
			response.setComplete();
            
		}
	}
};

class RequestThread : public Thread {
private:
	HttpClient<int> client;
	Url url;
public:
	RequestThread() {}
	virtual ~RequestThread() {}
	virtual void run() {
		printf("Request Start\n");
        try {
            client.request(url, "GET", NULL, 0, 0);
        } catch (IOException e) {
            cout << e.getMessage() << endl;
        }
		printf("Request Done\n");
	}

	void setUrl(string urlString) {
		url = Url(urlString);
	}

	void cancel() {
		client.disconnect();
	}
};

size_t readline(char * buffer, size_t max) {
	fgets(buffer, (int)max - 1, stdin);
	buffer[strlen(buffer) - 1] = 0;
	return strlen(buffer);
}

int main(int argc, char *args[]) {

	int port = 8082;
	HttpServer server(port);
	RequestThread rt;

	Hello hello;
	Late late;

	server.vpath("/hello/*", &hello);
	server.vpath("/late", &late);

	server.startAsync();

	cout << "start port: " << port << endl;

	while (1) {
		char buffer[1024] = {0,};
		readline(buffer, sizeof(buffer));
		if (!strcmp(buffer, "q")) {
			break;
		}

		if (!strcmp(buffer, "r")) {
			rt.setUrl("http://127.0.0.1:8082/late?timeout=10000");
			rt.start();
		}

		if (!strcmp(buffer, "s")) {
			rt.cancel();
		}
	}

	server.stop();

	std::cout << "Done" << std::endl;
	
    return 0;
}
