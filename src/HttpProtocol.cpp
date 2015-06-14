#include "HttpProtocol.hpp"
#include <iostream>

namespace HTTP {

	using namespace std;


	HttpRequest::HttpRequest() {
	}
	HttpRequest::~HttpRequest() {
	}
	
	string & HttpRequest::getMethod() {
	}
	string & HttpRequest::getPath() {
	}
	string & HttpRequest::getParamter(string & name) {
	}
	vector<string> & HttpRequest::getParamters(string & name) {
	}




	HttpResponse::HttpResponse() {
	}
	HttpResponse::~HttpResponse() {
	}
	
	int HttpResponse::write(std::string & content) {
	}

	


	
	HttpConnection::HttpConnection() {
	}
	HttpConnection::~HttpConnection() {
	}
	
	void HttpConnection::onConnect(MultiConnServer & server, OS::Socket & client) {
	}
	void HttpConnection::onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) {
		const char * data = "HTTP/1.1 200 OK\r\nContent-Length: 6\r\n\r\nfinish";
		buffer += string(packet.getBuffer());
		if (buffer.find("\r\n\r\n") != string::npos) {
			cout << " <<< " << buffer << endl;
			client.send((char*)data, strlen(data));
			server.disconnect(client);
		}
	}
	void HttpConnection::onDisconnect(MultiConnServer & server, OS::Socket & client) {
	}
	

	

	HttpProtocol::HttpProtocol() {
	}
	HttpProtocol::~HttpProtocol() {
	}
	
	void HttpProtocol::onConnect(MultiConnServer & server, OS::Socket & client) {
		conns[&client] = new HttpConnection();
	}
	void HttpProtocol::onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) {
		HttpConnection * conn = conns[&client];
		if (conn) {
			conn->onReceive(server, client, packet);
		}
	}
	void HttpProtocol::onDisconnect(MultiConnServer & server, OS::Socket & client) {
		delete conns[&client];
		conns.erase(&client);
	}
	
}
