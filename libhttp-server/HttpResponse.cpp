#include "HttpResponse.hpp"
#include "HttpStatusCodes.hpp"
#include "StringDataSource.hpp"
#include "FixedTransfer.hpp"
#include "FileDataSource.hpp"
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

    static AutoRef<Logger> logger = LoggerFactory::inst().getObservingLogger(__FILE__);

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
        _props.clear();
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
		return _props[name];
	}

	void HttpResponse::appendCookies(const vector<Cookie> & cookies) {
		for (vector<Cookie>::const_iterator iter = cookies.begin(); iter != cookies.end(); iter++) {
			appendCookie(*iter);
		}
	}

	void HttpResponse::appendCookie(const Cookie & cookie) {
		_header.appendHeaderField("Set-Cookie", cookie.toString());
	}

	void HttpResponse::setFixedTransfer(const string & content) {
		AutoRef<DataSource> source(new StringDataSource(content));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, content.size()));
        clearTransfer();
        setTransfer(transfer);
		setContentLength(content.size());
	}

	void HttpResponse::setFileTransfer(const string & filepath) {
		File file(filepath);
		setFileTransfer(file);
	}

	void HttpResponse::setFileTransfer(OS::File & file) {
		AutoRef<DataSource> source(new FileDataSource(FileStream(file, "rb")));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, (size_t)file.getSize()));
		clearTransfer();
		setTransfer(transfer);
		setContentLength(file.getSize());
	}

	void HttpResponse::setPartialFileTransfer(const HttpRange & r, const OS::File & file) {
        HttpRange range = r;
		size_t filesize = (size_t)file.getSize();
		if (filesize < 1) {
			throw Exception("Empty file");
		}
		range.adjustTo(filesize);
		if (range.from() >= range.to()) {
            throw Exception("Wrong range error - start (" + Text::toString(range.from()) +
                            ") bigger than end (" + Text::toString(range.to()) + ")");
		}
        logger->debug("Set Transfer Range: " + range.toString());
		FileStream stream(file, "rb");
		stream.seek(range.from());
		AutoRef<DataSource> source(new FileDataSource(stream));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, range.size(), 10 * 1024));
		setStatus(206);
		setContentLength(range.size());
		setRange(range, filesize);
		setTransfer(transfer);
	}

	void HttpResponse::setRange(const HttpRange & range, size_t filesize) {
		string bytes = "bytes ";
		bytes.append(range.toString());
		bytes.append("/");
		bytes.append(Text::toString(filesize));
		setHeaderField("Content-Range", bytes);
	}

}
