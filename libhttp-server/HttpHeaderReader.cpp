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
	void HttpHeaderReader::clear() {
		_buffer.clear();
	}
	bool HttpHeaderReader::complete() {
		return _buffer.find("\r\n\r\n") != string::npos;
	}
	int HttpHeaderReader::cutEndPos() {
		size_t f = _buffer.find("\r\n\r\n");
		size_t diff = _buffer.size() - (f + 4);
		_buffer = _buffer.substr(0, f + 4);
		return (int)diff;
	}
	void HttpHeaderReader::append(const char * data, size_t size) {
		string x = string(data, size);
		_buffer += x;
	}
	size_t HttpHeaderReader::read(const char * data, size_t size) {
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
		return parser.parse(_buffer) < 0 ? false : true;
	}
	HttpHeader & HttpHeaderReader::getHeader() {
		return parser.getHeader();
	}

	string & HttpHeaderReader::buffer() {
		return _buffer;
	}
}
