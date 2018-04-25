#include <liboslayer/Text.hpp>

#include "HttpHeaderParser.hpp"

namespace http {

	using namespace std;
	using namespace osl;

	/**
	 * @brief http header parse result constructor
	 */	
	HttpHeaderParseResult::HttpHeaderParseResult() : success(false), errorCode(0) {
	}
	HttpHeaderParseResult::HttpHeaderParseResult(bool success, int errorCode, string errorMessage)
		: success(false), errorCode(0), errorMessage(errorMessage) {
	}
	HttpHeaderParseResult::~HttpHeaderParseResult() {
	}

	void HttpHeaderParseResult::clear() {
		success = false;
		errorCode = 0;
		errorMessage.clear();
        header.clear();
	}

	int HttpHeaderParseResult::setResult(bool success, int errorCode, string errorMessage) {
		this->success = success;
		this->errorCode = errorCode;
		this->errorMessage = errorMessage;
		return errorCode;
	}
	HttpHeader & HttpHeaderParseResult::getHeader() {
		return header;
	}
	bool HttpHeaderParseResult::succeeded() {
		return success;
	}
	int HttpHeaderParseResult::getErrorCode() {
		return errorCode;
	}
	string HttpHeaderParseResult::getErrorMessage() {
		return errorMessage;
	}

	/**
	 * @brief http header parser constructor
	 */
	HttpHeaderParser::HttpHeaderParser() {
	}
	HttpHeaderParser::~HttpHeaderParser() {
	}
	int HttpHeaderParser::parse(const string & rawHeader) {

		result.clear();
		HttpHeader & httpHeader = result.getHeader();
		
		size_t f = 0;
		string line = readLine(rawHeader, f);
		if (line.empty()) {
			return result.setResult(false, -1, "invalid header");
		}

		if (parseFirstLine(httpHeader, line) < 0) {
			return result.setResult(false, -1, "invalid first line - " + line);
		}

		while (!isEmptyLine((line = readLine(rawHeader, f)))) {
			if (parseHeaderField(httpHeader, line) < 0) {
				return result.setResult(false, -1, "invalid param - " + line);
			}
		}
		return result.setResult(true, 0, "valid header");
	}
	bool HttpHeaderParser::isEmptyLine(string & line) {
		return (line.empty() || !line.compare("\r\n")) ? true : false;
	}
	string HttpHeaderParser::readLine(const string & full, size_t & f) {
		string ret;
		size_t nr = full.find("\r\n", f);
		if (nr != string::npos) {
			ret = full.substr(f, (nr + 2) - f);
			f = nr + 2;
		}
		return ret;
	}
	int HttpHeaderParser::parseFirstLine(HttpHeader & header, string & line) {
		string part1;
		string part2;
		string part3;
		string spaces = " \t";
		size_t e = 0;
		size_t f = line.find_first_of(spaces);
		if (f == string::npos) {
			return -1;
		}
		// first
		string part = line.substr(0, f);
		part1 = part;
		
		// second
		f = line.find_first_not_of(spaces, f);
		if (f == string::npos) {
			return -1;
		}
		e = line.find_first_of(spaces, f);
		part = (e != string::npos) ? line.substr(f, e - f) : line.substr(f);
		part2 = part;

		// third
		f = line.find_first_not_of(spaces, e);
		if (f == string::npos) {
			return -1;
		}
		e = line.find_first_of("\r\n", f);
		part = (e != string::npos) ? line.substr(f, e - f) : line.substr(f);
		part3 = part;

		header.setParts(part1, part2, part3);
		return 0;
	}
	int HttpHeaderParser::parseHeaderField(HttpHeader & header, string line) {
		line = removeCarriageReturnNewLine(line);
		size_t f = line.find(":");
		if (f == string::npos) {
			return -1;
		}
		string name = line.substr(0, f);
		string value = line.substr(f+1);
		name = Text::trim(name);
		value = Text::trim(value);
		header.setHeaderField(name, value);
		return 0;
	}
	HttpHeaderParseResult & HttpHeaderParser::getResult() {
		return result;
	}
	HttpHeader & HttpHeaderParser::getHeader() {
		return result.getHeader();
	}

	string HttpHeaderParser::removeCarriageReturnNewLine(string line) {
		size_t f = line.find("\r\n");
		if (f != string::npos) {
			line = line.substr(0, f);
		}
		return line;
	}
}
