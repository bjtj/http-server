#include <liboslayer/Text.hpp>
#include "HttpClient.hpp"
#include <iostream>

using namespace std;;
using namespace OS;
using namespace UTIL;

namespace HTTP {
	
	HttpClient::HttpClient() : responseHandler(NULL), socket(NULL) {
		defaultHeaderFields["User-Agent"] = "Cross-Platform/0.1 HTTP/1.1 HttpClient/0.1";
	}
	
	HttpClient::~HttpClient() {
	}

	void HttpClient::setHttpResponseHandler(HttpResponseHandler * responseHandler) {
		this->responseHandler = responseHandler;
	}

	void HttpClient::request(Url & url) {
		socket = connect(url);
		HttpHeader requestHeader = makeRequestHeader("GET", url.getPath(), "HTTP/1.1");
		sendRequestPacket(*socket, requestHeader, NULL, 0);
		HttpHeader responseHeader = readResponseHeader(*socket);
		if (responseHandler) {
			responseHandler->onResponse(*this, responseHeader, *socket);
		}
		disconnect(socket);
		socket = NULL;
	}

	Socket * HttpClient::connect(Url & url) {
		Socket * socket = new Socket(url.getHost().c_str(), url.getIntegerPort());
		socket->connect();
		return socket;
	}

	void HttpClient::disconnect(Socket * socket) {
		socket->close();
		delete socket;
	}

	HttpHeader HttpClient::makeRequestHeader(string method, string path, string protocol) {
		HttpHeader header;
		header.setPart1(method);
		header.setPart2(path);
		header.setPart3(protocol);
		// header.setHeaderField("", "");
		cout << "Header: " << header.toString();
		return header;
	}

	void HttpClient::sendRequestPacket(Socket & socket, HttpHeader & header, char * buffer, int len) {
		header.setHeaderField("Content-Length", Text::toString(len));
		string headerStr = header.toString();
		socket.send(headerStr.c_str(), headerStr.length());
		if (buffer && len > 0) {
			socket.send(buffer, len);
		}
	}

	HttpHeader HttpClient::readResponseHeader(Socket & socket) {
		HttpHeaderReader headerReader;
		char buffer;
		while (!headerReader.complete() && socket.recv(&buffer, 1) > 0) {
			headerReader.read(&buffer, 1);
		}
		return headerReader.getHeader();
	}
	
}
