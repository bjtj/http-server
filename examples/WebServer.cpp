#include <iostream>

#include <libhttp-server/HttpServer.hpp>
#include "HttpRequestHandlers.hpp"

using namespace std;
using namespace HTTP;

class MyListener : public OnConnectListener, public OnReceiveListener, public OnDisconnectListener
{
public:
    MyListener() {}
    virtual ~MyListener() {}

	// virtual void onConnect(MultiConnMultiplexServer & server, OS::Socket & client) {
	virtual void onConnect(MultiConn & server, ClientSession & client) {
		cout << " ++ " << client.getId() << endl;
	}

	// virtual void onReceive(MultiConnMultiplexServer & server, OS::Socket & client, Packet & packet) {
	virtual void onReceive(MultiConn & server, ClientSession & client, Packet & packet) {
		const char * data = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello";
		client.getSocket()->send((char*)data, strlen(data));
		server.disconnect(client);
	}

	// virtual void onDisconnect(MultiConnMultiplexServer & server, OS::Socket & client) {
	virtual void onDisconnect(MultiConn & server, ClientSession & client) {
		cout << " -- " << client.getId() << endl;
	}
};


static void s_run_multiconn() {
	
	bool done = false;
	int port = 8082;
	MultiConnMultiplexServer server(port);

	MyListener listener;
	server.setOnConnectListener(&listener);
	server.setOnReceiveListener(&listener);
	server.setOnDisconnectListener(&listener);
	
	server.start();

	while(!done) {
		server.poll(1000);
	}
	
	server.stop();
	cout << "bye~" << endl;
}

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

class ParamHandle : public OnHttpRequestHandler {
public:
    ParamHandle() {
	}
    virtual ~ParamHandle() {
	}

	virtual void onRequest(HttpRequest & request, HttpResponse & response) {

		if (!request.remaining()) {

			string method = request.getMethod();
			string name = request.getParameter("name");

			string content = string("<html><body>") +
				"<ul>" +
				"<li>method: " + method + "</li>" +
				"<li>param.name: " + name + "</li>" +
				"</ul>" +
				"</body></html>";
			
			response.write(content);
			
			response.setComplete();
		}
	}
};


/**
 * @brief run server
 */
static void s_run_server() {
	
	bool done = false;
	int port = 8082;
	
	MultiConnThreadedServer server(port);
	
	HttpProtocol http;
	
	FileRedirectHandler fileh(".");
	Hello hello;
	ParamHandle ph;
	http.vpath("/*", &fileh);
	http.vpath("/hello", &hello);
	http.vpath("/param", &ph);
	
	server.setProtocol(&http);
	server.start();

	std::cout << "Running: " << (server.isRunning() ? "running" : "stopped") << 
		", Listen port : " << port << std::endl;

	while(!done) {
		server.poll(1000);
	}
	
	server.stop();
	cout << "bye~" << endl;
}

/**
 * @brief main
 */
int main(int argc, char *args[]) {
	
	if (0) s_run_multiconn();
	if (1) s_run_server();
	
    return 0;
}
