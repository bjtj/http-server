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
}
