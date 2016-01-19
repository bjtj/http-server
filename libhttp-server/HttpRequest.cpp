#include <liboslayer/Text.hpp>

#include "HttpRequest.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	/**
	 * @brief http request
	 */
    HttpRequest::HttpRequest() {
    }
	HttpRequest::HttpRequest(HttpHeader & header)
		: header(header) {
	}
	HttpRequest::~HttpRequest() {
	}
    void HttpRequest::clear() {
        header.clear();
        clearTransfer();
    }
    void HttpRequest::setHeader(HttpHeader & header) {
        this->header.setHeader(header);
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
    vector<string> HttpRequest::getParameterNames() {
        return header.getParameterNames();
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
	const HttpRequestHeader & HttpRequest::getHeader() const {
		return header;
	}
	int HttpRequest::getContentLength() {
		return header.getContentLength();
	}
	string HttpRequest::getContentType() {
		return header.getContentType();
	}
    UTIL::AutoRef<DataTransfer> HttpRequest::getTransfer() {
        return transfer;
    }
    void HttpRequest::setTransfer(UTIL::AutoRef<DataTransfer> transfer) {
        this->transfer = transfer;
    }
    void HttpRequest::clearTransfer() {
        transfer = NULL;
    }
    void HttpRequest::setRemoteAddress(const OS::InetAddress & remoteAddress) {
        this->remoteAddress = remoteAddress;
    }
    OS::InetAddress & HttpRequest::getRemoteAddress() {
        return remoteAddress;
    }
}
