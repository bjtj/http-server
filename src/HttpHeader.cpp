#include <iostream>
#include "Text.hpp"
#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief http parameter constructor
	 */
	HttpParameter::HttpParameter() {
	}
	HttpParameter::HttpParameter(string name) : name(name){
	}
	HttpParameter::HttpParameter(string name, string value) : name(name){
		values.push_back(value);
	}
	HttpParameter::~HttpParameter() {
	}
	bool HttpParameter::empty() {
		return values.empty();
	}
	size_t HttpParameter::size() {
		return values.size();
	}
	string & HttpParameter::getName() {
		return name;
	}
	void HttpParameter::setName(string name) {
		this->name = name;
	}
	string HttpParameter::getFirstValue() {
		if (values.empty()) {
			return "";
		}
		return values[0];
	}
	vector<string> & HttpParameter::getValues() {
		return values;
	}
	string & HttpParameter::getValue(int index) {
		return values[index];
	}
	void HttpParameter::setValue(string value) {
		values.clear();
		values.push_back(value);
	}
	void HttpParameter::setValues(vector<string> & values) {
		this->values.clear();
		this->values = values;
	}
	void HttpParameter::appendValue(string value) {
		values.push_back(value);
	}
	void HttpParameter::appendValues(vector<string> & values) {
		this->values.insert(this->values.end(), values.begin(), values.end());
	}
	string & HttpParameter::operator[](int index) {
		return values[index];
	}
	string HttpParameter::toString() {
		string str;
		for (size_t i = 0; i < values.size(); i++) {
			if (i > 0) {
				str += "&";
			}
			str = name + "=" + values[i];
		}
		return str;
	}


	/**
	 * @brief http header constructor
	 */
	HttpHeader::HttpHeader() : valid(false) {
		parts.push_back("");
		parts.push_back("");
		parts.push_back("");
	}
	HttpHeader::~HttpHeader() {
	}
	bool HttpHeader::isValid() {
		return valid;
	}
	void HttpHeader::setFirstLine(string & firstline) {
		this->firstline = firstline;
	}
	void HttpHeader::setParts(vector<string> & parts) {
		this->parts = parts;
	}
	string HttpHeader::getPart1() {
		return parts[0];
	}
	string HttpHeader::getPart2() {
		return parts[1];
	}
	string HttpHeader::getPart3() {
		return parts[2];
	}
	void HttpHeader::setPart1(string part) {
		parts[0] = part;
	}
	void HttpHeader::setPart2(string part) {
		parts[1] = part;
	}
	void HttpHeader::setPart3(string part) {
		parts[2] = part;
	}
	string HttpHeader::getHeaderField(string name) {
		return fields[name];
	}
	void HttpHeader::setHeaderField(string name, string value) {
		fields[name] = value;
	}
	string HttpHeader::getParameter(string name) {
		return params[name].getFirstValue();
	}
	vector<string> HttpHeader::getParameters(string name) {
		return params[name].getValues();
	}
	void HttpHeader::setParameter(string name, string value) {
		if (params.find(name) == params.end()) {
			HttpParameter param(name);
			param.setValue(value);
			params[name] = param;
		}
		params[name].appendValue(value);
	}
	string HttpHeader::toString() {
		string ret = parts[0] + " " + parts[1] + " " +  parts[2] + "\r\n";
		for (map<string, string>::iterator iter = fields.begin(); iter != fields.end(); iter++) {
			ret += (iter->first + ": " + iter->second + "\r\n");
		}
		ret += "\r\n";
		return ret;
	}




	/**
	 * @brief http request header
	 */
	HttpRequestHeader::HttpRequestHeader(HttpHeader & header) : HttpHeaderWrapper(header) {
		parsePath();
	}

	HttpRequestHeader::~HttpRequestHeader() {
	}

	void HttpRequestHeader::parsePath() {
		HttpHeader & header = getHeader();
		string path = header.getPart2();
		size_t p = path.find("?");
		if (p != string::npos) {
			parseParams(path, ++p);
			header.setPart2(path.substr(0, p));
		}
	}
	void HttpRequestHeader::parseParams(const string & params, size_t offset) {
		HttpHeader & header = getHeader();
		HttpHeaderParser::parseParams(header, params, offset);
	}

	void HttpRequestHeader::setPart2(string part) {
		HttpHeaderWrapper::setPart2(part);
		parsePath();
	}
	
	string HttpRequestHeader::getMethod() {
		return getHeader().getPart1();
	}
	string HttpRequestHeader::getPath() {
		return getHeader().getPart2();
	}
	string HttpRequestHeader::getProtocol() {
		return getHeader().getPart3();
	}

	/**
	 * @brief http response header (wrapper) constructor
	 */
	HttpResponseHeader::HttpResponseHeader() : HttpHeaderWrapper(header) {
		header.setPart1("HTTP/1.1");
		header.setPart2("200");
		header.setPart3("OK");
	}
	HttpResponseHeader::HttpResponseHeader(HttpHeader & header) : HttpHeaderWrapper(header) {
	}
	HttpResponseHeader::~HttpResponseHeader() {
	}
	
	string HttpResponseHeader::getProtocol() {
		return getHeader().getPart1();
	}
	int HttpResponseHeader::getStatusCode() {
		return atoi(getHeader().getPart2().c_str());
	}
	void HttpResponseHeader::setStatusCode(int status) {
		getHeader().setPart2(UTIL::Text::toString(status));
	}
	string HttpResponseHeader::getMessage() {
		return getHeader().getPart3();
	}
	void HttpResponseHeader::setMessage(string message) {
		getHeader().setPart3(message);
	}


	

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
	size_t HttpHeaderParser::parseParam(HttpHeader & header, const string & param, size_t & f) {
		size_t sep = param.find("=", f);
		size_t next = param.find("&", f);
		if (sep != string::npos) {
			size_t s = sep + 1;
			size_t e = (next == string::npos ? param.length() : next);
			string name = param.substr(f, sep - f);
			string value = param.substr(s, e - s);
			header.setParameter(name, value);
			f = next + 1;
		}
		return next;
	}
	void HttpHeaderParser::parseParams(HttpHeader & header, std::string params, size_t offset) {
		size_t f = offset;
		while (parseParam(header, params, f) != string::npos) {
		}
	}
	int HttpHeaderParser::parseFirstLine(HttpHeader & header, string & line) {
		vector<string> parts;
		string spaces = " \t";
		size_t e = 0;
		size_t f = line.find_first_of(spaces);
		if (f == string::npos) {
			return -1;
		}
		// first
		string part = line.substr(0, f);
		parts.push_back(part);

		// second
		f = line.find_first_not_of(spaces, f);
		if (f == string::npos) {
			return -1;
		}
		e = line.find_first_of(spaces, f);
		part = (e != string::npos) ? line.substr(f, e - f) : line.substr(f);
		parts.push_back(part);

		// third
		f = line.find_first_not_of(spaces, e);
		if (f == string::npos) {
			return -1;
		}
		e = line.find_first_of("\r\n", f);
		part = (e != string::npos) ? line.substr(f, e - f) : line.substr(f);
		parts.push_back(part);

		header.setFirstLine(line);
		header.setParts(parts);
		return 0;
	}
	int HttpHeaderParser::parseHeaderField(HttpHeader & header, string line) {
		size_t f = line.find(":");
		if (f == string::npos) {
			return -1;
		}
		string name = line.substr(0, f);
		string value = line.substr(f+1);
		header.setParameter(Text::trim(name), Text::trim(value));
		return 0;
	}
	string HttpHeaderParser::readLine(const string & full, size_t & f) {
		string ret;
		size_t nr = full.find("\r\n", f);
		if (nr != string::npos) {
			ret = full.substr(f, nr + 2);
			f = nr + 2;
		}
		return ret;
	}
	int HttpHeaderParser::parse(const string & rawHeader) {

		HttpHeader & httpHeader = result.getHeader();
		
		size_t f = 0;
		string line = readLine(rawHeader, f);
		if (line.empty()) {
			return result.setResult(false, -1, "invalid header");
		}

		if (parseFirstLine(httpHeader, line) < 0) {
			return result.setResult(false, -1, "invalid first line - " + line);
		}

		while (!(line = readLine(rawHeader, f)).empty()) {
			if (parseHeaderField(httpHeader, line) < 0) {
				return result.setResult(false, -1, "invalid param - " + line);
			}
		}
		return result.setResult(true, 0, "valid header");
	}
	HttpHeaderParseResult & HttpHeaderParser::getResult() {
		return result;
	}
	HttpHeader & HttpHeaderParser::getHeader() {
		return result.getHeader();
	}


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
		return diff;
	}
	void HttpHeaderReader::append(char * data, int size) {
		string x = string(data, size);
		buffer += x;
	}
	bool HttpHeaderReader::parse() {
		return parser.parse(buffer) < 0 ? false : true;
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
	HttpHeader & HttpHeaderReader::getHeader() {
		return parser.getHeader();
	}
}
