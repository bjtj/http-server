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
	HttpResponse::HttpResponse(/*OS::Socket & socket*/)
		: /*socket(socket), */complete(false), headerSent(false), contentLength(-1) {
	}
	HttpResponse::~HttpResponse() {
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
	int HttpResponse::send(Socket & socket, const char * buf, int size) {
		if (contentLength > 0) {
			sendHeaderOnce(socket);
			return socket.send((char *)buf, size);
		}
		return 0;
	}
	int HttpResponse::write(const string & content) {
		this->content += content;
		return (int)content.length();
	}
	int HttpResponse::write(const char * buf, int size) {
		this->content += string(buf, size);
		return size;
	}
	void HttpResponse::sendHeaderOnce(Socket & socket) {
		if (!hasHeaderSent()) {
			string header_string = header.toString();
			socket.send((char*)header_string.c_str(), header_string.length());
			headerSent = true;
		}
	}
	bool HttpResponse::hasHeaderSent() {
		return headerSent;
	}
	void HttpResponse::sendContent(Socket & socket) {
        size_t contentLength = content.length();
		sendHeaderOnce(socket);
		int len = socket.send((char*)(content.c_str() + contentTranferCounter.getReadPosition()), contentTranferCounter.getReadSize(contentLength));
		if (len != contentLength) {
			logger.loge("send() error / expected: " + Text::toString(contentLength) + ", but: " + Text::toString(len));
		}
        contentTranferCounter.read(len);
		clearBuffer();
	}
	void HttpResponse::setComplete() {
		if (!complete) {
			setContentLength((int)content.length());
			complete = true;
		}
	}
	bool HttpResponse::hasComplete() {
		return complete;
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
