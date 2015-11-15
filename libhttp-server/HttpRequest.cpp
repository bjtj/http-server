#include <liboslayer/Text.hpp>

#include "HttpRequest.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	/**
	 * @brief http request
	 */
	HttpRequest::HttpRequest(HttpHeader & header, OS::Socket & socket)
		: header(header), socket(socket) {
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
    OS::Socket & HttpRequest::getSocket() {
        return socket;
    }
	int HttpRequest::getContentLength() {
		return header.getContentLength();
	}
	string HttpRequest::getContentType() {
		return header.getContentType();
	}
	void HttpRequest::setContentPacket(Packet & packet) {
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

			if (contentPacket.length() > 0) {
				buffer.readChunkData(contentPacket.getBuffer(), contentPacket.length());
				contentReadCounter.read(contentPacket.length());
			}
		}
	}

	bool HttpRequest::completeContentRead() {
		return contentReadCounter.complete();
	}
}
