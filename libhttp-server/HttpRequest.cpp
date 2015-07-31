#include "HttpRequest.hpp"
#include "Text.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	/**
	 * @brief http request
	 */
	HttpRequest::HttpRequest(HttpHeader & header, OS::Socket & socket)
		: header(header), contentSize(0), socket(socket) {
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
	map<string, string> & HttpRequest::getHeaderFields() {
		return header.getHeaderFields();
	}
	string HttpRequest::getParameter(const string & name) {
		return header.getParameter(name);
	}
	string HttpRequest::getParameter(const char * name) {
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
	void HttpRequest::compactContent() {
		content = "";
	}
	string & HttpRequest::getContent() {
		return content;
	}
	int HttpRequest::getContentLength() {
		return Text::toInt(header.getParameter("Content-Length"));
	}
	string HttpRequest::getContentType() {
		return header.getParameter("Content-Type");
	}
	bool HttpRequest::remaining() {
		return getContentLength() > contentSize;
	}
}
