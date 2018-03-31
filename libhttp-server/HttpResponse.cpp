#include <liboslayer/Text.hpp>
#include "HttpResponse.hpp"
#include "HttpStatusCodes.hpp"
#include "StringDataSource.hpp"
#include "FixedTransfer.hpp"
#include "FileDataSource.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	// Range::Range() : _from(0), _to(0) {
	// }
	// Range::Range(size_t from, size_t to) : _from(from), _to(to) {
	// }
	// Range::~Range() {
	// }
	// size_t & Range::from() {
	// 	return _from;
	// }
	// size_t & Range::to() {
	// 	return _to;
	// }
	// size_t Range::size() const {
	// 	return _to - _from;
	// }

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

	void HttpResponse::setPartialFileTransfer(const HttpRange & range, OS::File & file) {
		size_t to = range.to();
		if (file.getSize() < 1) {
			throw Exception("Empty file");
		}
		if (range.to() == 0 || range.to() >= (size_t)file.getSize()) {
			to = (size_t)file.getSize() - 1;
		}
		if (range.from() >= to) {
            throw Exception("Wrong range start error : end(" + Text::toString(to) +
                            ") but start(" + Text::toString(range.from()) + ")");
		}
		size_t size = (range.size() + 1);
		FileStream stream(file, "rb");
		stream.seek(range.from());
		AutoRef<DataSource> source(new FileDataSource(stream));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, size, 10 * 1024));
		setStatus(206);
		setContentLength(size);
		string bytes = "bytes=";
		bytes.append(Text::toString(range.from()));
		bytes.append("-");
		bytes.append(Text::toString(to));
		bytes.append("/");
		bytes.append(Text::toString(file.getSize()));
		setHeaderField("Content-Range", bytes);
		setTransfer(transfer);
	}

}
