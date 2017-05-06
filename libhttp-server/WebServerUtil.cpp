#include "WebServerUtil.hpp"
#include "StringDataSource.hpp"
#include "FixedTransfer.hpp"
#include "FileDataSource.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace OS;
	using namespace std;
	using namespace UTIL;

	Range::Range() : _from(0), _to(0) {
	}
	Range::Range(size_t from, size_t to) : _from(from), _to(to) {
	}
	Range::~Range() {
	}
	size_t & Range::from() {
		return _from;
	}
	size_t & Range::to() {
		return _to;
	}
	size_t Range::size() const {
		return _to - _from;
	}
	

	WebServerUtil::WebServerUtil() {
	}
	WebServerUtil::~WebServerUtil() {
	}
	void WebServerUtil::setFixedTransfer(HttpResponse & response, const string & content) {
		AutoRef<DataSource> source(new StringDataSource(content));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, content.size()));
        response.clearTransfer();
        response.setTransfer(transfer);
		response.setContentLength(content.size());
	}

	void WebServerUtil::setFileTransfer(HttpResponse & response, const string & filepath) {
		File file(filepath);
		setFileTransfer(response, file);
	}

	void WebServerUtil::setFileTransfer(HttpResponse & response, OS::File & file) {
		AutoRef<DataSource> source(new FileDataSource(FileStream(file, "rb")));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, (size_t)file.getSize()));
		response.clearTransfer();
		response.setTransfer(transfer);
		response.setContentLength(file.getSize());
	}

	void WebServerUtil::setPartialFileTransfer(HttpRequest & request, HttpResponse & response, OS::File & file) {
		string rangeParam = request.getHeaderFieldIgnoreCase("Range");
		if (rangeParam.empty()) {
			throw Exception("Range field empty");
		}
		Range range = parseRange(rangeParam);
		if (file.getSize() < 1) {
			throw Exception("Empty file");
		}
		if (range.to() == 0 || range.to() >= (size_t)file.getSize()) {
			range.to() = (size_t)file.getSize() - 1;
		}
		if (range.from() >= range.to()) {
            throw Exception("Wrong range start error : end(" + Text::toString(range.to()) +
                            ") but start(" + Text::toString(range.from()) + ")");
		}
		size_t size = (range.size() + 1);
		FileStream stream(file, "rb");
		stream.seek(range.from());
		AutoRef<DataSource> source(new FileDataSource(stream));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, size, 10 * 1024));
		response.setStatus(206);
		response.setContentLength(size);
		string bytes = "bytes=";
		bytes.append(Text::toString(range.from()));
		bytes.append("-");
		bytes.append(Text::toString(range.to()));
		bytes.append("/");
		bytes.append(Text::toString(file.getSize()));
		response.setHeaderField("Content-Range", bytes);
		response.setTransfer(transfer);
	}

	Range WebServerUtil::parseRange(const string & range) {
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
		return Range((size_t)Text::toLong(range.substr(s + 1, f)),
					 (size_t)Text::toLong(range.substr(f + 1)));
	}

}
