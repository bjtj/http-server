#include <liboslayer/Text.hpp>
#include "HttpRequest.hpp"
#include "StringDataSink.hpp"

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
	void HttpRequest::setPath(const string & path) {
		header.setPath(path);
	}
	string HttpRequest::getHeaderField(const string & name) const {
		return header.getHeaderField(name);
	}
	string HttpRequest::getHeaderFieldIgnoreCase(const string & name) const {
		return header.getHeaderFieldIgnoreCase(name);
	}
	LinkedStringListMap & HttpRequest::getHeaderFields() {
		return header.getHeaderFields();
	}
	map<string, string> HttpRequest::getHeaderFieldsStdMap() {
		return header.getHeaderFieldsStdMap();
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
	bool HttpRequest::isWwwFormUrlEncoded() {
		return (header.getContentType().empty() || header.getContentType() == "application/x-www-form-urlencoded");
	}
	void HttpRequest::parseWwwFormUrlencoded() {
		if (transfer.nil()) {
			return;
		}
		string data = ((StringDataSink*)&transfer->sink())->data();
		header.parseQuery(data);
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
	void HttpRequest::setLocalAddress(const OS::InetAddress & localAddress) {
		this->localAddress = localAddress;
	}
	OS::InetAddress & HttpRequest::getLocalAddress() {
		return localAddress;
	}
}
