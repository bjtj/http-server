#include "WebServerUtil.hpp"
#include "StringDataSource.hpp"
#include "FixedTransfer.hpp"
#include "FileDataSource.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace OS;
	using namespace std;
	using namespace UTIL;

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
		string range = request.getHeaderFieldIgnoreCase("Range");
		if (range.empty()) {
			throw Exception("Range field empty");
		}
		size_t start;
		size_t end;
		if (parseRange(range, start, end) == false) {
			throw Exception("Parse range failed");
		}
		if (file.getSize() < 1) {
			throw Exception("Empty file");
		}
		if (end == 0 || end >= (size_t)file.getSize()) {
			end = (size_t)file.getSize() - 1;
		}
		if (start >= end) {
            throw Exception("Wrong range start error : end(" + Text::toString(end) +
                            ") but start(" + Text::toString(start) + ")");
		}
		size_t size = (end - start + 1);
		FileStream stream(file, "rb");
		stream.seek(start);
		AutoRef<DataSource> source(new FileDataSource(stream));
		AutoRef<DataTransfer> transfer(new FixedTransfer(source, size, 10 * 1024));
		response.setStatus(206);
		response.setContentLength(size);
		string bytes = "bytes=";
		bytes.append(Text::toString(start));
		bytes.append("-");
		bytes.append(Text::toString(end));
		bytes.append("/");
		bytes.append(Text::toString(file.getSize()));
		response.setHeaderField("Content-Range", bytes);
		response.setTransfer(transfer);
	}

	bool WebServerUtil::parseRange(const string & range, size_t & from, size_t & to) {
		if (range.empty()) {
			return false;
		}
		size_t s = range.find("=");
		if (s == string::npos) {
			return false;
		}
		size_t f = range.find("-", s + 1);
		if (f == string::npos) {
			return false;
		}
		from = (size_t)Text::toLong(range.substr(s + 1, f));
		to = (size_t)Text::toLong(range.substr(f + 1));
		return true;
	}

}
