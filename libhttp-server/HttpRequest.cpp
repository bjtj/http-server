#include <liboslayer/Text.hpp>

#include "HttpRequest.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	/**
	 * @brief http request
	 */
    HttpRequest::HttpRequest() : contentPacket(NULL), transfer(NULL) {
    }
	HttpRequest::HttpRequest(HttpHeader & header)
		: header(header) {
	}
	HttpRequest::~HttpRequest() {
		if (transfer) {
			delete transfer;
		}
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
    ChunkedBuffer & HttpRequest::getChunkedBuffer() {
        return chunkedBuffer;
    }
    string & HttpRequest::getStringBuffer() {
        return stringBuffer;
    }
	int HttpRequest::getContentLength() {
		return header.getContentLength();
	}
	string HttpRequest::getContentType() {
		return header.getContentType();
	}
	void HttpRequest::setContentPacket(Packet * packet) {
		this->contentPacket = packet;
	}
	void HttpRequest::readChunkedBuffer(ChunkedBuffer & buffer) {

		int len = getContentLength();

		if (len > 0) {

			if (len != contentReadCounter.getContentSize()) {
				contentReadCounter.setContentSize(len);
			}

			if (len != buffer.getChunkSize()) {
				buffer.setChunkSize(len);
			}

			if (contentPacket->getLength() > 0) {
				buffer.write(contentPacket->getData(), contentPacket->getLength());
				contentReadCounter.read(contentPacket->getLength());
			}
		}
	}

	bool HttpRequest::completeContentRead() {
		return contentReadCounter.complete();
	}

	DataTransfer * HttpRequest::getTransfer() {
		return transfer;
	}
	void HttpRequest::setTransfer(DataTransfer * transfer) {
		this->transfer = transfer;
	}
}
