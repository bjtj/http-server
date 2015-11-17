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
	HttpResponse::HttpResponse()
    : complete(false), headerSent(false), contentLength(-1), transfer(NULL) {
	}
	HttpResponse::~HttpResponse() {
        if (transfer) {
            delete transfer;
        }
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
        
        contentTranferCounter.setContentSize(contentLength);
	}
	void HttpResponse::setContentType(string type) {
		header.setHeaderField("Content-Type", type);
	}
	void HttpResponse::clearBuffer() {
		content = "";
	}
    
    bool HttpResponse::completeContentTransfer() {
        return contentTranferCounter.complete();
    }
	HttpResponseHeader & HttpResponse::getHeader() {
		return header;
	}
	void HttpResponse::setTransfer(DataTransfer * transfer) {
        this->transfer = transfer;
    }
    DataTransfer * HttpResponse::getTransfer() {
        return transfer;
    }
}
