#include <liboslayer/Text.hpp>

#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief http header constructor
	 */
	HttpHeader::HttpHeader() : valid(false) {
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
		setPart1(parts[0]);
		setPart2(parts[1]);
		setPart3(parts[2]);
	}
	string HttpHeader::getPart1() {
		return part1;
	}
	string HttpHeader::getPart2() {
		return part2;
	}
	string HttpHeader::getPart3() {
		return part2;
	}
	void HttpHeader::setPart1(string part) {
		part1 = part;
	}
	void HttpHeader::setPart2(string part) {
		part2 = part;
	}
	void HttpHeader::setPart3(string part) {
		part3 = part;
	}
	string HttpHeader::makeFirstLine() {
		return (part1 + " " + part2 + " " + part3);
	}
	string HttpHeader::getHeaderField(string name) {
		return fields[name];
	}
	string HttpHeader::getHeaderFieldIgnoreCase(string name) {
		for (map<string, string>::iterator iter = fields.begin(); iter != fields.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->first, name)) {
				return iter->second;
			}
		}
		return "";
	}
	int HttpHeader::getHeaderFieldAsInteger(string name) {
		return Text::toInt(getHeaderField(name));
	}
	int HttpHeader::getHeaderFieldIgnoreCaseAsInteger(string name) {
		return Text::toInt(getHeaderFieldIgnoreCase(name));
	}
	void HttpHeader::setHeaderField(string name, string value) {
		fields[name] = value;
	}
	void HttpHeader::setHeaderFields(map<string, string> & fields) {
		this->fields = fields;
	}
	void HttpHeader::appendHeaderFields(map<string, string> & fields) {
		this->fields.insert(fields.begin(), fields.end());
	}
	map<string, string> & HttpHeader::getHeaderFields() {
		return fields;
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
		string ret = makeFirstLine() + "\r\n";
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
		parseParams(header, params, offset);
	}
	void HttpRequestHeader::parseParams(HttpHeader & header, string params, size_t offset) {
		size_t f = offset;
		while (parseParam(header, params, f) != string::npos) {}
	}
	size_t HttpRequestHeader::parseParam(HttpHeader & header, const string & param, size_t & f) {
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
	
}
