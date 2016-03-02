#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

#include "HttpResponse.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	const static Logger & logger = LoggerFactory::getDefaultLogger();

	/**
	 * @brief http response constructor
	 */
	HttpResponse::HttpResponse() : _needRedirect(false) {
	}
	HttpResponse::~HttpResponse() {
	}
    void HttpResponse::clear() {
        header.clear();
        clearTransfer();
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
		header.setContentLength(length);
	}
	void HttpResponse::setContentLength(size_t length) {
		header.setContentLength(length);
	}
	void HttpResponse::setContentLength(unsigned long long length) {
		header.setContentLength(length);
	}
	void HttpResponse::setContentType(string type) {
		header.setHeaderField("Content-Type", type);
	}
	HttpResponseHeader & HttpResponse::getHeader() {
		return header;
	}
    void HttpResponse::setHeader(HttpHeader & header) {
        this->header.setHeader(header);
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
		_needRedirect = true;
		redirectLocation = location;
	}

	string HttpResponse::getRedirectLocation() {
		return redirectLocation;
	}

	bool HttpResponse::needRedirect() {
		return _needRedirect;
	}
}
