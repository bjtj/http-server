#include "Cookie.hpp"
#include <vector>
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	Cookie::Cookie() {
	}
	Cookie::Cookie(const string & phrase) : _components(parse(phrase)) {
	}
	Cookie::Cookie(const LinkedStringMap & components) : _components(components) {
	}
	Cookie::~Cookie() {
	}
	LinkedStringMap & Cookie::components() {
		return _components;
	}
	bool Cookie::contains(const string & key) {
		return _components.contains(key);
	}
	bool Cookie::containsIgnoreCase(const string & key) {
		return _components.containsIgnoreCase(key);
	}
	string Cookie::toString() const {
		return toString(_components);
	}
	LinkedStringMap Cookie::parse(const string & phrase) {
		LinkedStringMap ret;
		vector<string> components = Text::split(phrase, ";");
		for (vector<string>::iterator iter = components.begin(); iter != components.end(); iter++) {
			string & component = *iter;
			size_t f = component.find("=");
			if (f == string::npos) {
				ret[Text::trim(component)] = "";
			} else {
				string key = Text::trim(component.substr(0, f));
				string value = Text::trim(component.substr(f+1));
				ret[key] = value;
			}
		}
		return ret;
	}
	string Cookie::toString(const LinkedStringMap & components) {
		string ret;
		for (size_t i = 0; i < components.size(); i++) {
			if (i > 0) {
				ret.append("; ");
			}
			ret.append(components[i].key());
			if (components[i].value().empty() == false) {
				ret.append("=");
				ret.append(components[i].value());
			}
		}
		return ret;
	}
	string & Cookie::operator[] (const string & name) {
		return _components[name];
	}
}
