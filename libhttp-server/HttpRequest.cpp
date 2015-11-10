#include <liboslayer/Text.hpp>

#include "HttpRequest.hpp"

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
	
	string HttpRequest::getMethod() const {
		return header.getMethod();
	}
	string HttpRequest::getPath() const {
		return header.getPath();
	}
	string & HttpRequest::getHeaderField(const string & name) {
		return header.getHeaderField(name);
	}
	string HttpRequest::getHeaderField(const string & name) const {
		return header.getHeaderField(name);
	}
	string & HttpRequest::getHeaderFieldIgnoreCase(const string & name) {
		return header.getHeaderFieldIgnoreCase(name);
	}
	string HttpRequest::getHeaderFieldIgnoreCase(const string & name) const {
		return header.getHeaderFieldIgnoreCase(name);
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
		return header.getContentLength();
	}
	string HttpRequest::getContentType() {
		return header.getContentType();
	}
	bool HttpRequest::remaining() {
		return getContentLength() > contentSize;
	}
}
