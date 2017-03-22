#include <liboslayer/Text.hpp>
#include "HttpResponse.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief http response constructor
	 */
	HttpResponse::HttpResponse()
		: HttpHeaderDelegator(_header), _redirectRequested(false), _forwardRequested(false) {
	}
	HttpResponse::~HttpResponse() {
	}
    void HttpResponse::clear() {
        _header.clear();
        clearTransfer();
		_redirectRequested = false;
		_redirectLocation.clear();
		_forwardRequested = false;
		_forwardLocation.clear();
    }
	void HttpResponse::setStatus(int statusCode) {
		_header.setStatus(statusCode);
	}
	void HttpResponse::setStatus(int statusCode, const string & statusString) {
		_header.setStatus(statusCode, statusString);
	}
	int HttpResponse::getStatusCode() {
		return _header.getStatusCode();
	}
	void HttpResponse::setParts(vector<string> & parts) {
		_header.setParts(parts);
	}
	
	HttpResponseHeader & HttpResponse::header() {
		return _header;
	}
	
    void HttpResponse::setTransfer(AutoRef<DataTransfer> transfer) {
        this->transfer = transfer;
    }
	
    AutoRef<DataTransfer> HttpResponse::getTransfer() {
        return transfer;
    }
	
	void HttpResponse::clearTransfer() {
        transfer = NULL;
	}

	void HttpResponse::setRedirect(const string & location) {
		_redirectRequested = true;
		_redirectLocation = location;
	}

	void HttpResponse::cancelRedirect() {
		_redirectRequested = false;
		_redirectLocation.clear();
	}

	void HttpResponse::setForward(const string & location) {
		_forwardRequested = true;
		_forwardLocation = location;
	}

	void HttpResponse::cancelForward() {
		_forwardRequested = false;
		_forwardLocation.clear();
	}

	string HttpResponse::getRedirectLocation() {
		return _redirectLocation;
	}

	string HttpResponse::getForwardLocation() {
		return _forwardLocation;
	}

	bool HttpResponse::redirectRequested() {
		return _redirectRequested;
	}

	bool HttpResponse::forwardRequested() {
		return _forwardRequested;
	}

	bool HttpResponse::isRedirectionStatus() {
		return _header.isRedirectionStatus();
	}

	string HttpResponse::getLocation() {
		return _header.getLocation();
	}

	string & HttpResponse::operator[] (const string & name) {
		return props[name];
	}

	void HttpResponse::setCookies(const vector<Cookie> & cookies) {
		for (vector<Cookie>::const_iterator iter = cookies.begin(); iter != cookies.end(); iter++) {
			_header.appendHeaderField("Set-Cookie", iter->toString());
		}
	}
}
