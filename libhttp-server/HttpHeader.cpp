#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/File.hpp>
#include "UrlEncoderDecoder.hpp"
#include "HttpHeader.hpp"
#include "HttpStatusCodes.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief HttpHeader
	 */
	HttpHeader::HttpHeader() {
	}
	
	HttpHeader::HttpHeader(const string & part1, const string & part2, const string & part3) {
		setParts(part1, part2, part3);
	}
	
	HttpHeader::~HttpHeader() {
	}

	string HttpHeader::getFirstLine() const {
		return (part1 + " " + part2 + " " + part3);
	}

	void HttpHeader::setHeader(const HttpHeader & other) {
		part1 = other.part1;
		part2 = other.part2;
		part3 = other.part3;
		fields = other.fields;
	}

	void HttpHeader::clear() {
		part1.clear();
		part2.clear();
		part3.clear();
		fields.clear();
	}
	
	void HttpHeader::setParts(const vector<string> & parts) {
		setPart1(parts[0]);
		setPart2(parts[1]);
		setPart3(parts[2]);
	}
	
	void HttpHeader::setParts(const string & part1, const string & part2, const string & part3) {
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
		return part3;
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
	
	bool HttpHeader::hasHeaderField(const string & name) const {
		return fields.contains(name);
	}
	
    string HttpHeader::getHeaderField(const string & name) const {
		for (size_t i = 0; i < fields.size(); i++) {
			if (fields[i].name() == name) {
				return fields[i].obj().first("");
			}
		}
		return "";
    }
	
	bool HttpHeader::hasHeaderFieldIgnoreCase(const string & name) const {
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
				return fields[i].obj().first("");
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
	
	void HttpHeader::setHeaderField(const string & name, const string & value) {
		fields[name].clearSet(value);
	}
	
	void HttpHeader::setHeaderField(const string & name, const StringList & value) {
		fields[name] = value;
	}
	
	void HttpHeader::setHeaderField(const string & name, const vector<string> & value) {
		fields[name] = value;
	}
	
	void HttpHeader::setHeaderFields(const map<string, string> & fields) {
		this->fields = fields;
	}

	void HttpHeader::appendHeaderField(const string & name, const string & value) {
		fields.append(name, value);
	}
	
	void HttpHeader::appendHeaderFields(const LinkedStringMap & fields) {
		this->fields.append(fields);
	}
	
	void HttpHeader::appendHeaderFields(const map<string, string> & fields) {
		this->fields.append(fields);
	}

	StringList HttpHeader::getHeaderFields(const string & name) {
		return fields[name];
	}
	
	LinkedStringListMap HttpHeader::getHeaderFields() {
		return fields;
	}
	
	map<string, string> HttpHeader::getHeaderFieldsStdMap() {
		return fields.to_first_map("");
	}
	
	void HttpHeader::removeHeaderField(const string & name) {
		fields.erase(name);
	}
	
	void HttpHeader::removeHeaderFieldIgnoreCase(const string & name) {
		fields.eraseIgnoreCase(name);
	}

	void HttpHeader::removeHeaderFields(const string & name) {
		fields.eraseAll(name);
	}
	
	void HttpHeader::removeHeaderFieldsIgnoreCase(const string & name) {
		fields.eraseAllIgnoreCase(name);
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
	
	void HttpHeader::setContentLength(long long contentLength) {
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
	
    void HttpHeader::setConnection(const string & connection) {
        setHeaderField("Connection", connection);
    }
    
    bool HttpHeader::keepConnection() {
        return Text::equalsIgnoreCase(getHeaderFieldIgnoreCase("Connection"), "keep-alive");
    }
	
	string HttpHeader::toString() const {
		string ret;
		ret.append(getFirstLine());
		ret.append("\r\n");
		for (size_t i = 0; i < fields.size(); i++) {
			for (size_t j = 0; j < fields[i].obj().size(); j++) {
				ret.append(fields[i].name());
				ret.append(": ");
				ret.append(fields[i].obj()[j]);
				ret.append("\r\n");
			}
		}
		ret.append("\r\n");
		return ret;
	}
	
	string & HttpHeader::operator[] (const string & fieldName) {
		for (size_t i = 0; i < fields.size(); i++) {
			if (fields[i].obj().size() > 0 && Text::equalsIgnoreCase(fields[i].name(), fieldName)) {
				return fields[i].obj().first();
			}
		}
		fields[fieldName].clearSet("");
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

	string HttpRequestHeader::getDirectory() const {
		return File::getDirectory(resourcePath);
	}
	
	string HttpRequestHeader::getRawPath() const {
		return getPart2();
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
		vector<KeyValue> kvs = parseSemiColonParameters(extractWithoutQuery(extractWithoutFragment(path)));
		setParameters(kvs);
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
	
	vector<KeyValue> HttpRequestHeader::parseSemiColonParameters(const string & path) {

		vector<KeyValue> kvs;
		
		size_t s = path.find(";");
		if (s == string::npos) {
			return kvs;
		}

		while (s != string::npos) {
			size_t f = path.find(";", s + 1);
			string part;
			if (f != string::npos) {
				part = path.substr(s + 1, f);
			} else {
				part = path.substr(s + 1);
			}
			KeyValue nv = parseKeyValue(part);
			kvs.push_back(nv);
			s = f;
		}

		return kvs;
	}
	
	KeyValue HttpRequestHeader::parseKeyValue(const string & text) {
		KeyValue kv;
		size_t f = text.find("=");
		if (f != string::npos) {
			kv.key() = text.substr(0, f);
			kv.value() = text.substr(f + 1);
		} else {
			kv.key() = text;
		}
		return kv;
	}
	
	void HttpRequestHeader::parseQuery(const string & query) {
		if (query.empty()) {
			return;
		}
		vector<string> queries = Text::split(query, "&");
		for (size_t i = 0; i < queries.size(); i++) {
			KeyValue kv = parseKeyValue(queries[i]);
			setParameter(kv.key(), UrlDecoder::decode_plus(kv.value()));
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

	void HttpRequestHeader::setParameters(vector<KeyValue> & kvs) {
		for (vector<KeyValue>::iterator iter = kvs.begin(); iter != kvs.end(); iter++) {
			setParameter(iter->key(), iter->value());
		}
	}

    void HttpRequestHeader::setHost(const string & host) {
        setHeaderField("Host", host);
    }

	/**
	 * @brief HttpResponseHeader
	 */
	
	HttpResponseHeader::HttpResponseHeader() {
		init();
	}
	
	HttpResponseHeader::HttpResponseHeader(int statusCode) {
		init();
		setStatus(statusCode);
	}
	
	HttpResponseHeader::HttpResponseHeader(int statusCode, const string & statusString) {
		init();
		setStatus(statusCode, statusString);
	}
	HttpResponseHeader::HttpResponseHeader(const HttpHeader & other) {
		setHeader(other);
	}
	HttpResponseHeader::~HttpResponseHeader() {
		/**/
	}
	void HttpResponseHeader::clear() {
		HttpHeader::clear();
		init();
	}
	void HttpResponseHeader::init() {
		setProtocol("HTTP/1.1");
	}
	string HttpResponseHeader::getProtocol() const {
		return getPart1();
	}
	void HttpResponseHeader::setProtocol(const string & protocol) {
		setPart1(protocol);
	}
	void HttpResponseHeader::setStatus(int statusCode) {
		setStatusCode(statusCode);
		setStatusString(HttpStatusCodes::getStatusString(statusCode));
	}
	void HttpResponseHeader::setStatus(int statusCode, const string & statusString) {
		setStatusCode(statusCode);
		setStatusString(statusString);
	}
	int HttpResponseHeader::getStatusCode() const {
		return Text::toInt(getPart2());
	}
	void HttpResponseHeader::setStatusCode(int statusCode) {
		setPart2(Text::toString(statusCode));
	}
	string HttpResponseHeader::getStatusString() const {
		return getPart3();
	}
	void HttpResponseHeader::setStatusString(const string & message)  {
		setPart3(message);
	}
    bool HttpResponseHeader::isRedirectionStatus() {
        return (getStatusCode() == 301 || getStatusCode() == 302);
    }
    string HttpResponseHeader::getLocation() {
        return getHeaderFieldIgnoreCase("Location");
    }
}
