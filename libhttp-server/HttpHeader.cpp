#include <liboslayer/Text.hpp>

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
	string & HttpHeader::getHeaderField(const string & name) {
		return fields[name];
	}
    string HttpHeader::getHeaderField(const std::string & name) const {
        for (map<string, string>::const_iterator iter = fields.begin(); iter != fields.end(); iter++) {
            if (!iter->first.compare(name)) {
                return iter->second;
            }
        }
        return "";
    }
	string & HttpHeader::getHeaderFieldIgnoreCase(const string & name) {
		for (map<string, string>::iterator iter = fields.begin(); iter != fields.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->first, name)) {
				return iter->second;
			}
		}
		return fields[name];
	}
	string HttpHeader::getHeaderFieldIgnoreCase(const string & name) const {
		for (map<string, string>::const_iterator iter = fields.begin(); iter != fields.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->first, name)) {
				return iter->second;
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
	void HttpHeader::appendHeaderFields(const map<string, string> & fields) {
		this->fields.insert(fields.begin(), fields.end());
	}
	map<string, string> & HttpHeader::getHeaderFields() {
		return fields;
	}
	void HttpHeader::removeHeaderField(string name) {
		fields.erase(name);
	}
	void HttpHeader::removeHeaderFieldIgnoreCase(string name) {
		for (map<string, string>::iterator iter = fields.begin(); iter != fields.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->first, name)) {
				fields.erase(iter);
				break;
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
	
	string HttpHeader::toString() const {
		string ret = makeFirstLine() + "\r\n";
		for (map<string, string>::const_iterator iter = fields.begin(); iter != fields.end(); iter++) {
			ret += (iter->first + ": " + iter->second + "\r\n");
		}
		ret += "\r\n";
		return ret;
	}
	string & HttpHeader::operator[] (const string & headerFieldName) {
		return getHeaderFieldIgnoreCase(headerFieldName);
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
        parsePath(getPath());
    }

	string HttpRequestHeader::getMethod() const {
		return getPart1();
	}
	void HttpRequestHeader::setMethod(const string & method) {
		setPart1(method);
	}
	string HttpRequestHeader::getPath() const {
		return getPart2();
	}
	void HttpRequestHeader::setPath(const string & path) {
		parsePath(path);
	}
	string HttpRequestHeader::getProtocol() const {
		return getPart3();
	}
	void HttpRequestHeader::setProtocol(const string & protocol) {
		setPart3(protocol);
	}
	void HttpRequestHeader::parsePath(const string & path) {

		size_t sep = path.find("?");
		if (sep != string::npos) {
			setPart2(path.substr(0, sep));
			parseQuery(path.substr(sep + 1));
		} else {
			setPart2(path);
		}
	}
	void HttpRequestHeader::parseQuery(const string & query) {
		params.clear();
		if (query.empty()) {
			return;
		}
		vector<string> queries = Text::split(query, "&");
		for (size_t i = 0; i < queries.size(); i++) {
			string q = queries[i];
			size_t sep = q.find("=");
			if (sep != string::npos) {
				setParameter(q.substr(0, sep), q.substr(sep + 1));
			} else {
				setParameter(q, "");
			}
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




	/**
	 * @brief HttpResponseHeader
	 */


	HttpResponseHeader::HttpResponseHeader() {
		setProtocol("HTTP/1.1");
		setStatusCode(200);
		setMessage("OK");
	}
	HttpResponseHeader::HttpResponseHeader(const HttpHeader & other) {
		setHeader(other);
	}
	HttpResponseHeader::~HttpResponseHeader() {
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

}
