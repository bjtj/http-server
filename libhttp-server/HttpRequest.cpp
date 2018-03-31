#include <liboslayer/Text.hpp>
#include "HttpRequest.hpp"
#include "StringDataSink.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;
	
	/**
	 * @brief http request
	 */
    HttpRequest::HttpRequest() : HttpHeaderDelegator(_header) {
    }
	
	HttpRequest::~HttpRequest() {
	}
	
    void HttpRequest::clear() {
        _header.clear();
        clearTransfer();
    }
    void HttpRequest::setHeader(const HttpHeader & header) {
        _header.setHeader(header);
    }
	string HttpRequest::getMethod() const {
		return _header.getMethod();
	}
	string HttpRequest::getProtocol() const {
		return _header.getProtocol();
	}
	void HttpRequest::setHost(const std::string & host) {
		_header.setHost(host);
	}
	string HttpRequest::getPath() const {
		return _header.getPath();
	}
	void HttpRequest::setPath(const string & path) {
		_header.setPath(path);
	}
	string HttpRequest::getDirectory() const {
		return _header.getDirectory();
	}
	string HttpRequest::getRawPath() const {
		return _header.getRawPath();
	}

    vector<string> HttpRequest::getParameterNames() {
        return _header.getParameterNames();
    }

	bool HttpRequest::hasParameter(const string & name) {
		return _header.hasParameter(name);
	}
	
	string HttpRequest::getParameter(const string & name) {
		return _header.getParameter(name);
	}
	
	string HttpRequest::getParameter(const char * name) {
		return _header.getParameter(name);
	}
	
	vector<string> HttpRequest::getParameters(const string & name) {
		return _header.getParameters(name);
	}
	
	HttpRequestHeader & HttpRequest::header() {
		return _header;
	}

	bool HttpRequest::isWwwFormUrlEncoded() {
		return (_header.getContentType().empty()
				|| _header.getContentType() == "application/x-www-form-urlencoded");
	}
	void HttpRequest::parseWwwFormUrlencoded() {
		if (transfer.nil()) {
			return;
		}
		string data = ((StringDataSink*)&transfer->sink())->data();
		_header.parseQuery(data);
	}
	
    AutoRef<DataTransfer> HttpRequest::getTransfer() {
        return transfer;
    }
	
    void HttpRequest::setTransfer(AutoRef<DataTransfer> transfer) {
        this->transfer = transfer;
    }
	
    void HttpRequest::clearTransfer() {
        transfer = NULL;
    }
	
    void HttpRequest::setRemoteAddress(const InetAddress & remoteAddress) {
        this->remoteAddress = remoteAddress;
    }
	
    InetAddress & HttpRequest::getRemoteAddress() {
        return remoteAddress;
    }
	
	void HttpRequest::setLocalAddress(const InetAddress & localAddress) {
		this->localAddress = localAddress;
	}
	
	InetAddress & HttpRequest::getLocalAddress() {
		return localAddress;
	}

	vector<Cookie> HttpRequest::getCookies() {
		vector<Cookie> cookies;
		StringList lst = _header.getHeaderFields("Cookie");
		for (size_t i = 0; i < lst.size(); i++) {
			cookies.push_back(Cookie(lst[i]));
		}
		return cookies;
	}

	string HttpRequest::getCookie(const string & key) {
		vector<Cookie> cookies = getCookies();
		for (vector<Cookie>::iterator iter = cookies.begin(); iter != cookies.end(); iter++) {
			Cookie & cookie = *iter;
			if (cookie[key].empty() == false) {
				return cookie[key];
			}
		}
		return "";
	}

	HttpRange HttpRequest::getRange() {
		string rangeParam = getHeaderFieldIgnoreCase("Range");
		if (rangeParam.empty()) {
			throw Exception("Range field empty");
		}
		return parseRange(rangeParam);
	}

	HttpRange HttpRequest::parseRange(const string & range) {
		if (range.empty()) {
			throw Exception("empty string");
		}
		size_t s = range.find("=");
		if (s == string::npos) {
			throw Exception("'=' is missing");
		}
		size_t f = range.find("-", s + 1);
		if (f == string::npos) {
			throw Exception("'-' is missing");
		}
		return HttpRange((size_t)Text::toLong(range.substr(s + 1, f)),
						 (size_t)Text::toLong(range.substr(f + 1)));
	}
}
