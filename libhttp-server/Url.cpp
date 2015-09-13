#include <liboslayer/Text.hpp>

#include "Url.hpp"

namespace URL {

	using namespace std;
	
	Url::Url() {
	}
	Url::~Url() {
	}

	string Url::getScheme() {
		return scheme;
	}
	string Url::getHost() {
		return host;
	}
	string Url::getPort() {
		return port;
	}
	string Url::getPath() {
		return path;
	}
	void Url::setScheme(string scheme) {
		this->scheme = scheme;
	}
	void Url::setHost(string host) {
		this->host = host;
	}
	void Url::setPort(string port) {
		this->port = port;
	}
	void Url::setPath(string path) {
		this->path = path;
	}

	void Url::appendParam(string name, string value) {
		params.push_back(make_pair(name, value));
	}
	
	vector<pair<string, string> > & Url::getParams() {
		return params;
	}

	string Url::toString() {
		string p;
		p = UTIL::Text::toMapString(params, "=", "&");
		if (!p.empty()) {
			p = "?" + p;
		}

		scheme + "://" + host + "/" + path + p;
	}

}
