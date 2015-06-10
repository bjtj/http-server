#include "HttpHeader.hpp"

namespace HTTP {

	using namespace std;
	
	HeaderField::HeaderField(string name) {
	}
	HeaderField::~HeaderField() {
	}

	bool HeaderField::empty() {
	}
	size_t HeaderField::size() {
	}
	string & HeaderField::getName() {
	}
	string & HeaderField::getFirstParameter() {
	}
	vector<string> & HeaderField::getParameters() {
	}
	string & HeaderField::getParameter(int index) {
	}
	void HeaderField::setParameter(string param) {
	}
	void HeaderField::setParameters(vector<string> & params) {
	}
	void HeaderField::appendParamter(string param) {
	}
	void HeaderField::appendParameters(vector<string> & param) {
	}

	string & HeaderField::operator[](int index) {
	}
	string HeaderField::toString() {
	}


	Header::Header() {
	}
	Header::~Header() {
	}

	bool Header::contains() {
	}
	string Header::getParameter(string name) {
	}
	vector<string> Header::getParameters(string name) {
	}

	HeaderParseResult::HeaderParseResult() {
	}
	HeaderParseResult::~HeaderParseResult() {
	}

	Header & HeaderParseResult::getHeader() {
	}
	bool HeaderParseResult::isSucceeded() {
	}
	bool HeaderParseResult::isFailed() {
	}
	int HeaderParseResult::getErrorCode() {
	}
	string HeaderParseResult::getErrorMessage() {
	}

	HeaderParser::HeaderParser() {
	}
	HeaderParser::~HeaderParser() {
	}

	HeaderParseResult HeaderParser::parse(string & header) {
	}
}
