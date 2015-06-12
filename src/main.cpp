#include <iostream>
#include "HttpServer.hpp"
#include "MultiConn.hpp"

using namespace std;
using namespace HTTP;

class MyListener : public OnConnectListener, public OnReceiveListener
{
public:
    MyListener() {}
    virtual ~MyListener() {}

	virtual void onConnect(OS::Socket & client) {
		cout << "connected: " << client.getFd() << endl;
	}

	virtual void onReceive(OS::Socket & client, Packet & packet) {
		const char * data = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello";
		client.send((char*)data, strlen(data));
	}
};


static void s_run_multiconn() {
	bool done = false;
	MultiConnServer server(8082);

	MyListener listener;
	server.setOnConnectListener(&listener);
	server.setOnReceiveListener(&listener);
	
	server.start();

	while(!done) {
		server.poll(1000);
	}
	
	server.stop();
	cout << "bye~" << endl;
}

/**
 * @brief run server
 */
static void s_run_server() {
	
	HttpServer server(8080);

	server.start();
	
	server.stop();
}

/**
 * @brief main
 */
int main(int argc, char *args[]) {
	
	//s_run_server();
	s_run_multiconn();
	
    return 0;
}
