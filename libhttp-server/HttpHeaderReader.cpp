#include <liboslayer/Text.hpp>

#include "HttpHeaderReader.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief http header reader constructor
	 */
	HttpHeaderReader::HttpHeaderReader() {
	}
	HttpHeaderReader::~HttpHeaderReader() {
	}
	bool HttpHeaderReader::complete() {
		return buffer.find("\r\n\r\n") != string::npos;
	}
	int HttpHeaderReader::cutEndPos() {
		size_t f = buffer.find("\r\n\r\n");
		size_t diff = buffer.size() - (f + 4);
		buffer = buffer.substr(0, f + 4);
		return (int)diff;
	}
	void HttpHeaderReader::append(char * data, int size) {
		string x = string(data, size);
		buffer += x;
	}
	int HttpHeaderReader::read(char * data, int size) {
		if (complete()) {
			return 0;
		}
		append(data, size);
		if (complete()) {
			int diff = cutEndPos();
			return parse() ? diff : -1;
		}
		return size;
	}
	bool HttpHeaderReader::parse() {
		return parser.parse(buffer) < 0 ? false : true;
	}
	HttpHeader & HttpHeaderReader::getHeader() {
		return parser.getHeader();
	}
}
