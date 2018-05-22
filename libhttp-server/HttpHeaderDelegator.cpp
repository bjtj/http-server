#include "HttpHeaderDelegator.hpp"


namespace http {

	using namespace std;
	using namespace osl;


	HttpHeaderDelegator::HttpHeaderDelegator(HttpHeader & header)
		: _header(header) {
	}

	HttpHeaderDelegator::~HttpHeaderDelegator() {
	}

	void HttpHeaderDelegator::clear() {
		_header.clear();
	}

	void HttpHeaderDelegator::setParts(const vector<string> & parts) {
		_header.setParts(parts);
	}
	void HttpHeaderDelegator::setParts(const string & part1, const string & part2, const string & part3) {
		_header.setParts(part1, part2, part3);
	}
	string HttpHeaderDelegator::HttpHeaderDelegator::getPart1() const {
		return _header.getPart1();
	}
	string HttpHeaderDelegator::HttpHeaderDelegator::getPart2() const {
		return _header.getPart2();
	}
	string HttpHeaderDelegator::HttpHeaderDelegator::getPart3() const {
		return _header.getPart3();
	}
	void HttpHeaderDelegator::setPart1(const string & part) {
		_header.setPart1(part);
	}
	void HttpHeaderDelegator::setPart2(const string & part) {
		_header.setPart2(part);
	}
	void HttpHeaderDelegator::setPart3(const string & part) {
		_header.setPart3(part);
	}
	bool HttpHeaderDelegator::hasHeaderFieldCase(const string & name) const {
		return _header.hasHeaderFieldCase(name);
	}
	string HttpHeaderDelegator::getHeaderFieldCase(const string & name) const {
		return _header.getHeaderFieldCase(name);
	}
	bool HttpHeaderDelegator::hasHeaderField(const string & name) const {
		return _header.hasHeaderField(name);
	}
	string HttpHeaderDelegator::getHeaderField(const string & name) const {
		return _header.getHeaderField(name);
	}
	int HttpHeaderDelegator::getHeaderFieldCaseAsInteger(const string & name) const {
		return _header.getHeaderFieldCaseAsInteger(name);
	}
	int HttpHeaderDelegator::getHeaderFieldAsInteger(string name) const {
		return _header.getHeaderFieldAsInteger(name);
	}
	void HttpHeaderDelegator::setHeaderField(const string & name, const string & value) {
		_header.setHeaderField(name, value);
	}
	void HttpHeaderDelegator::setHeaderField(const string & name, const StringList & value) {
		_header.setHeaderField(name, value);
	}
	void HttpHeaderDelegator::setHeaderField(const string & name, const vector<string> & value) {
		_header.setHeaderField(name, value);
	}
	void HttpHeaderDelegator::setHeaderFields(const map<string, string> & fields) {
		_header.setHeaderFields(fields);
	}
	void HttpHeaderDelegator::appendHeaderField(const string & name, const string & value) {
		_header.appendHeaderField(name, value);
	}
	void HttpHeaderDelegator::appendHeaderFields(const LinkedStringMap & fields) {
		_header.appendHeaderFields(fields);
	}
	void HttpHeaderDelegator::appendHeaderFields(const map<string, string> & fields) {
		_header.appendHeaderFields(fields);
	}
	StringList HttpHeaderDelegator::getHeaderFields(const string & name) {
		return _header.getHeaderFields(name);
	}
	LinkedStringListMap HttpHeaderDelegator::getHeaderFields() {
		return _header.getHeaderFields();
	}
	map<string, string> HttpHeaderDelegator::getHeaderFieldsStdMap() {
		return _header.getHeaderFieldsStdMap();
	}
	void HttpHeaderDelegator::removeHeaderFieldCase(const string & name) {
		_header.removeHeaderFieldCase(name);
	}
	void HttpHeaderDelegator::removeHeaderField(const string & name) {
		_header.removeHeaderField(name);
	}
	void HttpHeaderDelegator::removeHeaderFieldsCase(const string & name) {
		_header.removeHeaderFieldsCase(name);
	}
	void HttpHeaderDelegator::removeHeaderFields(const string & name) {
		_header.removeHeaderFields(name);
	}

	/* HTTP */
	
	string HttpHeaderDelegator::getContentType() const {
		return _header.getContentType();
	}
	void HttpHeaderDelegator::setContentType(string contentType) {
		_header.setContentType(contentType);
	}
	int HttpHeaderDelegator::getContentLength() const {
		return _header.getContentLength();
	}
	void HttpHeaderDelegator::setContentLength(long long contentLength) {
		_header.setContentLength(contentLength);
	}
	bool HttpHeaderDelegator::isChunkedTransfer() const {
		return _header.isChunkedTransfer();
	}
	void HttpHeaderDelegator::setChunkedTransfer(bool chunked) {
		_header.setChunkedTransfer(chunked);
	}
	void HttpHeaderDelegator::setConnection(const string & connection) {
		_header.setConnection(connection);
	}
	bool HttpHeaderDelegator::keepConnection() {
		return _header.keepConnection();
	}
	string HttpHeaderDelegator::toString() const {
		return _header.toString();
	}
}
