#include "Text.hpp"
#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

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


	

	HttpHeader::HttpHeader() : valid(false) {
	}
	HttpHeader::~HttpHeader() {
	}
	bool HttpHeader::isValid() {
		return valid;
	}
	void HttpHeader::setFirstLine(std::string & firstline) {
		this->firstline = firstline;
	}
	void HttpHeader::setParts(std::vector<std::string> & parts) {
		this->parts = parts;
	}
	std::string HttpHeader::getPart1() {
		return parts[0];
	}
	std::string HttpHeader::getPart2() {
		return parts[1];
	}
	std::string HttpHeader::getPart3() {
		return parts[2];
	}
	string HttpHeader::getHeaderField(string name) {
		return fields[name];
	}
	void HttpHeader::setHeaderField(std::string name, std::string value) {
		fields[name] = value;
	}
	string HttpHeader::getParameter(string name) {
		return params[name].getFirstValue();
	}
	vector<string> HttpHeader::getParameters(string name) {
		return params[name].getValues();
	}
	void HttpHeader::setParameter(std::string name, std::string value) {
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
	

	
	HttpHeaderParseResult::HttpHeaderParseResult() : success(false), errorCode(0) {
	}
	HttpHeaderParseResult::HttpHeaderParseResult(bool success, int errorCode, std::string errorMessage)
		: success(false), errorCode(0), errorMessage(errorMessage) {
	}
	HttpHeaderParseResult::~HttpHeaderParseResult() {
	}
	int HttpHeaderParseResult::setResult(bool success, int errorCode, std::string errorMessage) {
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


	
	HttpHeaderParser::HttpHeaderParser() {
	}
	HttpHeaderParser::~HttpHeaderParser() {
	}
	int HttpHeaderParser::parseFirstLine(HttpHeader & header, std::string & line) {
		vector<string> parts;
		string space = " \t";
		size_t e = 0;
		size_t f = line.find_first_of(space);
		if (f == string::npos) {
			return -1;
		}
		string part = line.substr(0, f);
		parts.push_back(part);
		f = line.find_first_not_of(space, f);
		if (f == string::npos) {
			return -1;
		}
		e = line.find_first_of(space, f);
		part = (e != string::npos) ? line.substr(f, e - f) : line.substr(f);
		parts.push_back(part);

		f = line.find_first_not_of(space, e);
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
	int HttpHeaderParser::parseParam(HttpHeader & header, string line) {
		size_t f = line.find(":");
		if (f == string::npos) {
			return -1;
		}
		string name = line.substr(0, f);
		string value = line.substr(f+1);
		header.setParameter(Text::trim(name), Text::trim(value));
		return 0;
	}
	int HttpHeaderParser::parse(string & header) {
		size_t f = header.find("\r\n");
		if (f == string::npos) {
			return result.setResult(false, -1, "invalid header");
		}
		string line = header.substr(0, f + 2);
		if (parseFirstLine(result.getHeader(), line) < 0) {
			return result.setResult(false, -1, "invalid first line - " + line);
		}
		size_t s = f + 2;
		while ((f = header.find("\r\n", s)) != string::npos) {
			line = header.substr(s, f - s);
			if (line.empty()) {
				break;
			}
			if (parseParam(result.getHeader(), line) < 0) {
				return result.setResult(false, -1, "invalid param - " + line);
			}
			s = f + 2;
		}
		return result.setResult(true, 0, "valid header");
	}
	HttpHeaderParseResult & HttpHeaderParser::getResult() {
		return result;
	}
	HttpHeader & HttpHeaderParser::getHeader() {
		return result.getHeader();
	}


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
