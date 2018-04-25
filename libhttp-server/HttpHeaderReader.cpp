#include <liboslayer/Text.hpp>

#include "HttpHeaderReader.hpp"

namespace http {

	using namespace std;
	using namespace osl;

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
	void HttpHeaderReader::append(const char * data, size_t size) {
		_buffer.append(data, size);
	}
	size_t HttpHeaderReader::read(const char * data, size_t size) {
		append(data, size);
		return size;
	}
	bool HttpHeaderReader::parse() {
		return parser.parse(_buffer) < 0 ? false : true;
	}
	HttpHeader & HttpHeaderReader::getHeader() {
		parse();
		return parser.getHeader();
	}
	string & HttpHeaderReader::buffer() {
		return _buffer;
	}
}
