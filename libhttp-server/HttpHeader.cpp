#include <iostream>
#include <liboslayer/Text.hpp>
#include "HttpEncoderDecoder.hpp"
#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	/**
	 * @brief HttpHeader
	 */
	HttpHeader::HttpHeader() : valid(false) {
	}
	HttpHeader::HttpHeader(std::string part1, std::string part2, std::string part3) : valid(false) {
		setParts(part1, part2, part3);
	}
	HttpHeader::~HttpHeader() {
	}

	bool HttpHeader::isValid() {
		return valid;
	}
	void HttpHeader::clear() {
		valid = false;
		firstline.clear();
		part1.clear();
		part2.clear();
		part3.clear();
		fields.clear();
	}

	void HttpHeader::setHeader(const HttpHeader & other) {
		valid = other.valid;
		firstline = other.firstline;
		part1 = other.part1;
		part2 = other.part2;
		part3 = other.part3;
		fields = other.fields;
	}

	void HttpHeader::setFirstLine(string & firstline) {
		this->firstline = firstline;
	}
	void HttpHeader::setParts(vector<string> & parts) {
		setPart1(parts[0]);
		setPart2(parts[1]);
		setPart3(parts[2]);
	}
	void HttpHeader::setParts(std::string part1, std::string part2, std::string part3) {
		setPart1(part1);
		setPart2(part2);
		setPart3(part3);
	}
	string HttpHeader::getPart1() const {
		return part1;
	}
	string HttpHeader::getPart2() const {
		return part2;
	}
	string HttpHeader::getPart3() const {
		return part2;
	}
	void HttpHeader::setPart1(const string & part) {
		part1 = part;
	}
	void HttpHeader::setPart2(const string & part) {
		part2 = part;
	}
	void HttpHeader::setPart3(const string & part) {
		part3 = part;
	}
	string HttpHeader::makeFirstLine() const {
		return (part1 + " " + part2 + " " + part3);
	}
	bool HttpHeader::hasHeaderField(const std::string & name) const {
		return fields.contains(name);
	}
    string HttpHeader::getHeaderField(const std::string & name) const {
		for (size_t i = 0; i < fields.size(); i++) {
			if (fields[i].name() == name) {
				return fields[i].first_safe("");
			}
		}
		return "";
    }
	bool HttpHeader::hasHeaderFieldIgnoreCase(const std::string & name) const {
		for (size_t i = 0; i < fields.size(); i++) {
			if (Text::equalsIgnoreCase(fields[i].name(), name)) {
				return true;
			}
		}
		return false;
	}
	string HttpHeader::getHeaderFieldIgnoreCase(const string & name) const {
		for (size_t i = 0; i < fields.size(); i++) {
			if (Text::equalsIgnoreCase(fields[i].name(), name)) {
				return fields[i].first_safe("");
			}
		}
		return "";
	}
	int HttpHeader::getHeaderFieldAsInteger(string name) const {
		return Text::toInt(getHeaderField(name));
	}
	int HttpHeader::getHeaderFieldIgnoreCaseAsInteger(string name) const {
		return Text::toInt(getHeaderFieldIgnoreCase(name));
	}
	void HttpHeader::setHeaderField(string name, string value) {
		fields[name] = value;
	}
	void HttpHeader::setHeaderFields(map<string, string> & fields) {
		this->fields = fields;
	}
	void HttpHeader::appendHeaderFields(const LinkedStringMap & fields) {
		this->fields.append(fields);
	}
	void HttpHeader::appendHeaderFields(const map<string, string> & fields) {
		this->fields.append(fields);
	}
	LinkedStringListMap & HttpHeader::getHeaderFields() {
		return fields;
	}
	map<string, string> HttpHeader::getHeaderFieldsStdMap() {
		return fields.toStdMap();
	}
	void HttpHeader::removeHeaderField(const string & name) {
		fields.erase(name);
	}
	void HttpHeader::removeHeaderFieldIgnoreCase(const string & name) {
		for (size_t i = 0; i < fields.size(); i++) {
			if (Text::equalsIgnoreCase(fields[i].name(), name)) {
				fields.erase(fields[i].name());
				return;
			}
		}
	}
	string HttpHeader::getContentType() const {
		return getHeaderFieldIgnoreCase("Content-Type");
	}
	void HttpHeader::setContentType(string contentType) {
		setHeaderField("Content-Type", contentType);
	}
	int HttpHeader::getContentLength() const {
		return getHeaderFieldIgnoreCaseAsInteger("Content-Length");
	}
	void HttpHeader::setContentLength(int contentLength) {
		setHeaderField("Content-Length", Text::toString(contentLength));
	}
	void HttpHeader::setContentLength(size_t contentLength) {
		setHeaderField("Content-Length", Text::toString(contentLength));
	}
	void HttpHeader::setContentLength(unsigned long long contentLength) {
		setHeaderField("Content-Length", Text::toString(contentLength));
	}
	bool HttpHeader::isChunkedTransfer() const {
		return Text::equalsIgnoreCase(getHeaderFieldIgnoreCase("Transfer-Encoding"), "chunked");
	}
	void HttpHeader::setChunkedTransfer(bool chunked) {
		if (chunked) {
			setHeaderField("Transfer-Encoding", "chunked");
		} else {
			removeHeaderFieldIgnoreCase("Transfer-Encoding");
		}
	}
    void HttpHeader::setConnection(const std::string & connection) {
        setHeaderField("Connection", connection);
    }
    
    bool HttpHeader::keepConnection() {
        return Text::equalsIgnoreCase(getHeaderFieldIgnoreCase("Connection"), "keep-alive");
    }
	
	string HttpHeader::toString() const {
		string ret = makeFirstLine() + "\r\n";
		for (size_t i = 0; i < fields.size(); i++) {
			ret += (fields[i].name() + ": " + fields[i].first_safe("") + "\r\n");
		}
		ret += "\r\n";
		return ret;
	}
	string & HttpHeader::operator[] (const string & fieldName) {
		for (size_t i = 0; i < fields.size(); i++) {
			if (fields[i].size() > 0 && Text::equalsIgnoreCase(fields[i].name(), fieldName)) {
				return fields[i].first();
			}
		}
		fields[fieldName] = "";
		return fields[fieldName].first();
	}

	/**
	 * @brief HttpRequestHeader
	 */

	HttpRequestHeader::HttpRequestHeader() {
	}
	HttpRequestHeader::HttpRequestHeader(const HttpHeader & other) {
		setHeader(other);
	}
	HttpRequestHeader::~HttpRequestHeader() {
	}
	void HttpRequestHeader::clear() {
		HttpHeader::clear();
		params.clear();
	}
    void HttpRequestHeader::setHeader(const HttpHeader & other) {
        HttpHeader::setHeader(other);
        parsePath(getPart2());
    }
	string HttpRequestHeader::getMethod() const {
		return getPart1();
	}
	void HttpRequestHeader::setMethod(const string & method) {
		setPart1(method);
	}
	string HttpRequestHeader::getPath() const {
		return resourcePath;
	}
	void HttpRequestHeader::setPath(const string & path) {
		setPart2(path);
		parsePath(path);
	}
	string HttpRequestHeader::getProtocol() const {
		return getPart3();
	}
	void HttpRequestHeader::setProtocol(const string & protocol) {
		setPart3(protocol);
	}
	void HttpRequestHeader::parsePath(const string & path) {

		params.clear();
		
		resourcePath = extractResourcePath(path);
		vector<NameValue> nvs = parseSemiColonParameters(extractWithoutQuery(extractWithoutFragment(path)));
		setParameters(nvs);
		parseQuery(extractQuery(extractWithoutFragment(path)));
		fragment = extractFragment(path);
	}
	string HttpRequestHeader::extractResourcePath(const string & path) {
		string ret = extractWithoutFragment(path);
		ret = extractWithoutQuery(ret);
		ret = extractWithoutSemicolon(ret);
		return ret;
	}
	string HttpRequestHeader::extractWithoutSemicolon(const string & path) {
		size_t s = path.find(";");
		if (s != string::npos) {
			return path.substr(0, s);
		}
		return path;
	}
	string HttpRequestHeader::extractWithoutQuery(const string & path) {
		size_t s = path.find("?");
		if (s != string::npos) {
			return path.substr(0, s);
		}
		return path;
	}
	string HttpRequestHeader::extractQuery(const string & path) {
		size_t s = path.find("?");
		if (s != string::npos) {
			return extractWithoutFragment(path.substr(s + 1));
		}
		return "";
	}
	string HttpRequestHeader::extractWithoutFragment(const string & path) {
		size_t s = path.find("#");
		if (s != string::npos) {
			return path.substr(0, s);
		}
		return path;
	}
	string HttpRequestHeader::extractFragment(const string & path) {
		size_t s = path.find("#");
		if (s != string::npos) {
			return path.substr(s + 1);
		}
		return "";
	}
	vector<NameValue> HttpRequestHeader::parseSemiColonParameters(const string & path) {

		vector<NameValue> nvs;
		
		size_t s = path.find(";");
		if (s == string::npos) {
			return nvs;
		}

		while (s != string::npos) {
			size_t f = path.find(";", s + 1);
			string part;
			if (f != string::npos) {
				part = path.substr(s + 1, f);
			} else {
				part = path.substr(s + 1);
			}
			NameValue nv = parseNameValue(part);
			nvs.push_back(nv);
			s = f;
		}

		return nvs;
	}
	NameValue HttpRequestHeader::parseNameValue(const string & text) {
		NameValue nv;
		size_t f = text.find("=");
		if (f != string::npos) {
			nv.name() = text.substr(0, f);
			nv.value() = text.substr(f + 1);
		} else {
			nv.name() = text;
		}
		return nv;
	}
	void HttpRequestHeader::parseQuery(const string & query) {
		if (query.empty()) {
			return;
		}
		vector<string> queries = Text::split(query, "&");
		for (size_t i = 0; i < queries.size(); i++) {
			NameValue nv = parseNameValue(queries[i]);
			setParameter(nv.name(), HttpDecoder::decode(HttpDecoder::decode_plus(nv.value())));
		}
	}
    vector<string> HttpRequestHeader::getParameterNames() {
        vector<string> names;
        for (map<string, HttpParameter>::const_iterator iter = params.begin(); iter != params.end(); iter++) {
            names.push_back(iter->first);
        }
        return names;
    }
	string HttpRequestHeader::getParameter(string name) {
		return params[name].getFirstValue();
	}
	vector<string> HttpRequestHeader::getParameters(string name) {
		return params[name].getValues();
	}
	void HttpRequestHeader::setParameter(string name, string value) {
		if (params.find(name) == params.end()) {
			HttpParameter param(name);
			param.setValue(value);
			params[name] = param;
		}
		params[name].appendValue(value);
	}

	void HttpRequestHeader::setParameters(vector<NameValue> & nvs) {
		for (vector<NameValue>::iterator iter = nvs.begin(); iter != nvs.end(); iter++) {
			setParameter(iter->name(), iter->value());
		}
	}

    void HttpRequestHeader::setHost(const std::string & host) {
        setHeaderField("Host", host);
    }

	/**
	 * @brief HttpResponseHeader
	 */
	
	HttpResponseHeader::HttpResponseHeader() {
		setProtocol("HTTP/1.1");
	}
	HttpResponseHeader::HttpResponseHeader(const HttpHeader & other) {
		setHeader(other);
	}
	HttpResponseHeader::~HttpResponseHeader() {
	}

	void HttpResponseHeader::clear() {
		HttpHeader::clear();
		setProtocol("HTTP/1.1");
	}
	string HttpResponseHeader::getProtocol() const {
		return getPart1();
	}
	void HttpResponseHeader::setProtocol(const string & protocol) {
		setPart1(protocol);
	}
	int HttpResponseHeader::getStatusCode() const {
		return Text::toInt(getPart2());
	}
	void HttpResponseHeader::setStatusCode(int statusCode) {
		setPart2(Text::toString(statusCode));
	}
	std::string HttpResponseHeader::getMessage() const {
		return getPart3();
	}
	void HttpResponseHeader::setMessage(const std::string & message)  {
		setPart3(message);
	}
    bool HttpResponseHeader::isRedirection() {
        return (getStatusCode() == 301 || getStatusCode() == 302);
    }
    string HttpResponseHeader::getRedirectionLocation() {
        return getHeaderFieldIgnoreCase("Location");
    }
}
