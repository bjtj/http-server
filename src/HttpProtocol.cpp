#include <iostream>
#include "HttpProtocol.hpp"
#include "Text.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	HttpRequest::HttpRequest(HttpHeader & header, OS::Socket & socket) : header(header), socket(socket) {
	}
	HttpRequest::~HttpRequest() {
	}
	
	string HttpRequest::getMethod() {
		return header.getMethod();
	}
	string HttpRequest::getPath() {
		return header.getPath();
	}
	string HttpRequest::getHeaderField(string & name) {
		return header.getHeaderField(name);
	}
	string HttpRequest::getParameter(string & name) {
		return header.getParameter(name);
	}
	vector<string> HttpRequest::getParameters(string & name) {
		return header.getParameters(name);
	}
	HttpRequestHeader & HttpRequest::getHeader() {
		return header;
	}
	void HttpRequest::appendContent(char * buffer, size_t size) {
		content += string(buffer, size);
	}
	void HttpRequest::clearContent() {
		content = "";
	}


	HttpResponse::HttpResponse(OS::Socket & socket) : socket(socket) {
	}
	HttpResponse::~HttpResponse() {
	}
	
	int HttpResponse::write(std::string content) {
		socket.send((char*)content.c_str(), content.length());
	}

	


	
	HttpConnection::HttpConnection(HttpProtocol & protocol) : protocol(protocol) {
	}
	HttpConnection::~HttpConnection() {
	}
	
	void HttpConnection::onConnect(MultiConnServer & server, OS::Socket & client) {
	}
	void HttpConnection::onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) {

		if (!headerReader.complete()) {
			int offset = headerReader.read(packet.getBuffer(), packet.size());
			gatherContent(packet.getBuffer() + offset, packet.length() - offset);
		} else {
			gatherContent(packet.getBuffer(), packet.length());
		}

		if (headerReader.complete()) {
			HttpRequest request(headerReader.getHeader(), client);
			HttpResponse response(client);
			onRequest(request, response);

			server.disconnect(client);
		}
		
		// buffer += string(packet.getBuffer());
		// if (buffer.find("\r\n\r\n") != string::npos) {
		// 	HttpHeaderParser parser;
		// 	if (parser.parse(buffer) < 0) {
		// 		cout << "error : " << parser.getResult().getErrorMessage() << endl;
		// 		const char * data = "HTTP/1.1 200 OK\r\nContent-Length: 6\r\n\r\nfinish";
		// 		client.send((char*)data, strlen(data));
		// 	} else {
		// 		HttpHeader & header = parser.getHeader();
		// 		string path = header.getPart2();
		// 		string content = "Path is " + path;
		// 		string p = "HTTP/1.1 200 OK\r\nContent-Length: " + Text::toString(content.length()) +
		// 			"\r\n\r\n" + content;
		// 		cout << "path : " << path << endl;
		// 		client.send((char*)p.c_str(), p.length());
		// 	}

		// 	server.disconnect(client);
		// }
	}
	void HttpConnection::onDisconnect(MultiConnServer & server, OS::Socket & client) {
	}
	void HttpConnection::gatherContent(char * buffer, size_t size) {
	}
	void HttpConnection::onRequest(HttpRequest & request, HttpResponse & response) {
		protocol.onRequest(request, response);
	}
	bool HttpConnection::needMoreHeader() {
		return !headerReader.complete();
	}
	void HttpConnection::readHeader(Packet & packet) {
	}
	bool HttpConnection::needMoreContent() {
	}
	void HttpConnection::readContent(Packet & packet) {
	}
	
	

	HttpProtocol::HttpProtocol() {
	}
	HttpProtocol::~HttpProtocol() {
	}
	
	void HttpProtocol::onConnect(MultiConnServer & server, OS::Socket & client) {
		conns[&client] = new HttpConnection(*this);
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
	void HttpProtocol::vpath(std::string path, HttpRequestHandler * handler) {
		if (!handler) {
			handlers.erase(path);
		} else {
			handlers[path] = handler;
		}
	}
	void HttpProtocol::onRequest(HttpRequest & request, HttpResponse & response) {
		HttpRequestHandler * handler = getHandler(request.getPath());
		if (handler) {
			handler->onRequest(request, response);
		} else {
			response.write("HTTP/1.1 404 not found\r\n\r\n");
		}
	}
	HttpRequestHandler * HttpProtocol::getHandler(std::string path) {
		return handlers[path];
	}
}
