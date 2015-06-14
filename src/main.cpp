#include <iostream>
#include "HttpProtocol.hpp"
#include "MultiConn.hpp"

using namespace std;
using namespace HTTP;

class MyListener : public OnConnectListener, public OnReceiveListener, public OnDisconnectListener
{
public:
    MyListener() {}
    virtual ~MyListener() {}

	virtual void onConnect(MultiConnServer & server, OS::Socket & client) {
		cout << " ++ " << client.getFd() << endl;
	}

	virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) {
		const char * data = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello";
		client.send((char*)data, strlen(data));
		server.disconnect(client);
	}

	virtual void onDisconnect(MultiConnServer & server, OS::Socket & client) {
		cout << " -- " << client.getFd() << endl;
	}
};


static void s_run_multiconn() {
	bool done = false;
	MultiConnServer server(8082);

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

class MyPath : public HttpRequestHandler
{
public:
    MyPath() {
	}
    virtual ~MyPath() {
	}

	virtual void onRequest(HttpRequest & request, HttpResponse & response) {
		response.write("HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello world");
	}
};


/**
 * @brief run server
 */
static void s_run_server() {
	
	bool done = false;
	
	MultiConnServer server(8082);
	
	HttpProtocol http;
	MyPath mypath;
	http.vpath("/", &mypath);
	
	server.setProtocol(&http);
	server.start();

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
	
	if (1) s_run_server();
	if (0) s_run_multiconn();
	
    return 0;
}
