#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	
	HttpHeaderField::HttpHeaderField(string name) {
	}
	HttpHeaderField::~HttpHeaderField() {
	}

	bool HttpHeaderField::empty() {
	}
	size_t HttpHeaderField::size() {
	}
	string & HttpHeaderField::getName() {
	}
	string & HttpHeaderField::getFirstParameter() {
	}
	vector<string> & HttpHeaderField::getParameters() {
	}
	string & HttpHeaderField::getParameter(int index) {
	}
	void HttpHeaderField::setParameter(string param) {
	}
	void HttpHeaderField::setParameters(vector<string> & params) {
	}
	void HttpHeaderField::appendParamter(string param) {
	}
	void HttpHeaderField::appendParameters(vector<string> & param) {
	}

	string & HttpHeaderField::operator[](int index) {
	}
	string HttpHeaderField::toString() {
	}


	HttpHeader::HttpHeader() {
	}
	HttpHeader::~HttpHeader() {
	}

	bool HttpHeader::contains() {
	}
	string HttpHeader::getParameter(string name) {
	}
	vector<string> HttpHeader::getParameters(string name) {
	}

	HttpHeaderParseResult::HttpHeaderParseResult() {
	}
	HttpHeaderParseResult::~HttpHeaderParseResult() {
	}

	HttpHeader & HttpHeaderParseResult::getHeader() {
	}
	bool HttpHeaderParseResult::isSucceeded() {
	}
	bool HttpHeaderParseResult::isFailed() {
	}
	int HttpHeaderParseResult::getErrorCode() {
	}
	string HttpHeaderParseResult::getErrorMessage() {
	}

	HttpHeaderParser::HttpHeaderParser() {
	}
	HttpHeaderParser::~HttpHeaderParser() {
	}

	HttpHeaderParseResult HttpHeaderParser::parse(string & header) {
	}
}
