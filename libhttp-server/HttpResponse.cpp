#include "HttpResponse.hpp"
#include "Text.hpp"

#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief http response constructor
	 */
	HttpResponse::HttpResponse(OS::Socket & socket)
		: socket(socket), complete(false), headerSent(false), contentLength(-1) {
	}
	HttpResponse::~HttpResponse() {
	}
	void HttpResponse::setStatusCode(int code) {
		header.setStatusCode(code);
		header.setMessage(HttpStatusCodes::getMessage(code));
	}
	void HttpResponse::setStatusCode(int code, string message) {
		header.setStatusCode(code);
		header.setMessage(message);
	}
	void HttpResponse::setParts(vector<string> & parts) {
		header.setParts(parts);
	}
	void HttpResponse::setContentLength(int length) {
		contentLength = length;
		header.setHeaderField("Content-Length", Text::toString(length));
	}
	void HttpResponse::setContentType(string type) {
		header.setHeaderField("Content-Type", type);
	}
	void HttpResponse::clearBuffer() {
		content = "";
	}
	int HttpResponse::send(const char * buf, int size) {
		if (contentLength > 0) {
			sendHeaderOnce();
			return socket.send((char *)buf, size);
		}
		return 0;
	}
	int HttpResponse::write(const string & content) {
		this->content += content;
		return content.length();
	}
	int HttpResponse::write(const char * buf, int size) {
		this->content += string(buf, size);
		return size;
	}
	void HttpResponse::sendHeaderOnce() {
		if (!headerSent) {
			string header_string = header.toString();
			socket.send((char*)header_string.c_str(), header_string.length());
			headerSent = true;
		}
	}
	void HttpResponse::sendContent() {
		socket.send((char*)content.c_str(), content.length());
		clearBuffer();
	}
	void HttpResponse::setComplete() {
		if (!complete) {
			setContentLength(content.length());
			sendHeaderOnce();
			sendContent();
			complete = true;
		}
	}
	bool HttpResponse::hasComplete() {
		return complete;
	}

}
