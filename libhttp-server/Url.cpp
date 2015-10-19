#include <liboslayer/Text.hpp>

#include "Url.hpp"

namespace HTTP {

	using namespace std;

	Url::Url(const char * urlStr) {
		parseUrlString(urlStr);
	}
	Url::Url(const string & urlStr) {
		parseUrlString(urlStr);
	}
	
	Url::~Url() {
	}

	string Url::getScheme() {
		return scheme;
	}
	string Url::getProtocol() {
		return scheme;
	}
	string Url::getHost() {
		return host;
	}
	string Url::getPort() {
		return port;
	}
	int Url::getIntegerPort() {
		return UTIL::Text::toInt(port);
	}
	string Url::getPath() {
		return path;
	}
	void Url::setScheme(string scheme) {
		this->scheme = scheme;
	}
	void Url::setProtocol(string scheme) {
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

	void Url::parseUrlString(string urlStr) {

		clear();
		
		size_t f = urlStr.find("://");
		if (f == string::npos) {
			throw -1;
		}
		scheme = urlStr.substr(0, f);
		urlStr = urlStr.substr(f + 3);
		f = urlStr.find("/");
		if (f != string::npos) {
			vector<string> addr = parseAddress(urlStr.substr(0, f));
			setAddress(addr);
			path = urlStr.substr(f);
		} else {
			vector<string> addr = parseAddress(urlStr);
			setAddress(addr);
			path = "/";
		}
	}

	vector<string> Url::parseAddress(string address) {
		vector<string> ret;
		size_t f = address.find(":");
		if (f != string::npos) {
			ret.push_back(address.substr(0, f));
			ret.push_back(address.substr(f + 1));
		} else {
			ret.push_back(address);
		}
		return ret;
	}

	void Url::setAddress(vector<string> & addr) {
		host = addr[0];
		if (addr.size() > 1) {
			port = addr[1];
		} else {
			port = "80";
		}
	}

	string Url::toString() {
		string p;
		p = UTIL::Text::toMapString(params, "=", "&");
		if (!p.empty()) {
			p = "?" + p;
		}

		scheme + "://" + host + "/" + path + p;
	}

	Url& Url::operator=(const char * urlStr) {
		this->parseUrlString(urlStr);
		return *this;
	}

	Url& Url::operator=(const string & urlStr) {
		this->parseUrlString(urlStr);
		return *this;
	}

	void Url::clear() {
		scheme = "";
		host = "";
		port = "";
		path = "";
		params.clear();
	}

}
