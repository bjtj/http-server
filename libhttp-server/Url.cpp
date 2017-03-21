#include "Url.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
    using namespace UTIL;

	static vector<string> createSchemes() {
		vector<string> schemes;
		schemes.push_back("http");
		schemes.push_back("https");
		schemes.push_back("file");
		return schemes;
	}

	vector<string> Url::knownSchemes = createSchemes();

    Url::Url() {
    }
	
	Url::Url(const char * urlStr) {
		parseUrlString(urlStr);
	}
	
	Url::Url(const string & urlStr) {
		parseUrlString(urlStr);
	}
	
	Url::~Url() {
	}

	string Url::getUsername() const {
		return username;
	}
	string Url::getPassword() const {
		return password;
	}
	string Url::getScheme() const {
		return scheme;
	}
	string Url::getProtocol() const {
		return scheme;
	}
	string Url::getHost() const {
		return host;
	}
	string Url::getPort() const {
		return port;
	}
	int Url::getIntegerPort() const {
		return UTIL::Text::toInt(port);
	}
	string Url::getPath() const {
		return path;
	}
    string Url::getPathAndQuery() const {
        string query = getQueryString();
        if (!query.empty()) {
            query = "?" + query;
        }
        return path + query;
    }
    string Url::getPathWithoutPrefix(const string & prefix) {
        if (Text::startsWith(this->path, prefix)) {
            return this->path.substr(prefix.length());
        }
        return this->path;
    }
    string Url::getQueryString() const {
        return Text::toString(parameters.const_elements(), "=", "&");
    }
	void Url::getUsername(const string & username) {
		this->username = username;
	}
	void Url::getPassword(const string & password) {
		this->password = password;
	}
	void Url::setScheme(const string & scheme) {
		this->scheme = scheme;
	}
	void Url::setProtocol(const string & scheme) {
		this->scheme = scheme;
	}
	void Url::setHost(const string & host) {
		this->host = host;
	}
	void Url::setPort(const string & port) {
		this->port = port;
	}
	void Url::setPath(const string & path) {
        string p = path;
        if (!Text::startsWith(p, "/")) {
            p = "/" + p;
        }
		this->path = p;
		parsePath(this->path);
	}
    void Url::setRelativePath(const string & relativePath) {
        
        if (Text::startsWith(relativePath, "/")) {
			setPath(relativePath);
        } else {
            size_t l = path.find_last_of("/");
            if (l == string::npos) {
				setPath(relativePath);
            } else {
                string prefix = path.substr(0, l+1);
				setPath(prefix + relativePath);
            }
        }
    }

	Url Url::relativePath(const string & relativePath) const {
		Url u = *this;
		u.setRelativePath(relativePath);
		return u;
	}

	void Url::setParameter(const string & name, const string & value) {
        parameters[name] = value;
	}
	
	LinkedStringMap & Url::getParameters() {
		return parameters;
	}
    
    void Url::setUrl(const string & urlStr) {
        parseUrlString(urlStr);
    }

	void Url::parseUrlString(const string & urlStr) {

		string u = urlStr;
		clear();
		
		size_t f = urlStr.find("://");
		if (f == string::npos) {
			throw WrongUrlFormatException("no protocol found");
		}

		scheme = u.substr(0, f);

		u = u.substr(f + 3);
		f = u.find("/");
		if (f != string::npos) {
			parseAddress(u.substr(0, f));
			parsePath(u.substr(f));
		} else {
			parseAddress(u);
			path = "/";
		}
		if (port.empty()) {
			port = Text::toString(getKnownPort(scheme));
		}
	}

	void Url::parseAddress(const string & address) {

		string temp = address;
		
		size_t at = temp.find("@");
		if (at != string::npos) {
			string auth = temp.substr(0, at);
			size_t s = auth.find(":");
			if (s != string::npos) {
				username = auth.substr(0, s);
				password = auth.substr(s + 1);
			} else {
				username = auth;
			}
			temp = temp.substr(at + 1);
		}
		
		size_t f = temp.find(":");
		if (f != string::npos) {
			host = temp.substr(0, f);
			port = temp.substr(f + 1);
		} else {
			host = temp;
			port = "";
		}
	}

	void Url::parsePath(const string & resource) {
		size_t q = resource.find("?");
		if (q != string::npos) {
			string query = resource.substr(q + 1);
			path = resource.substr(0, q);
			parseQuery(query);
		} else {
			path = resource;
			parameters.clear();
		}
	}
    
    void Url::parseQuery(const string & query) {
        parameters.clear();
        if (query.empty()) {
            return;
        }
        
        vector<string> queries = Text::split(query, "&");
        for (size_t i = 0; i < queries.size(); i++) {
            string & q = queries[i];
            size_t f = q.find("=");
            
            if (f == string::npos) {
                string name = q;
                string value;
                
                setParameter(name, value);
            } else {
                string name = q.substr(0, f);
                string value = q.substr(f+1);
                
                setParameter(name, value);
            }
        }
    }

	string Url::getAddress() const {
		return host + (port.empty() ? "" : ":" + port);
	}

	int Url::getKnownPort(const std::string & scheme) {
		if (scheme == "http") {
			return 80;
		} else if (scheme == "https") {
			return 443;
		}
		return 0;
	}

	bool Url::isKnownScheme(const string & scheme) {
		for (vector<string>::iterator iter = knownSchemes.begin(); iter != knownSchemes.end(); iter++) {
			if (*iter == scheme) {
				return true;
			}
		}
		return false;
	}
    
	string Url::toString() const {

		string ret;

		ret.append(scheme);
		ret.append("://");

		if (!username.empty()) {
			ret.append(username);
			ret.append(":");
			ret.append(password);
			ret.append("@");
		}
		
		ret.append(host);
		ret.append(":");
		ret.append(port);
        
        string p = getPathAndQuery();
        if (Text::startsWith(p, "/")) {
            p = p.substr(1);
        }

		ret.append("/");
		ret.append(p);

		return ret;
	}

	void Url::validateUrlFormat(const string & url) {
		size_t f = url.find("://");
		if (f == string::npos) {
			throw WrongUrlFormatException("no protocol found");
		}
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
		scheme.clear();
		host.clear();
		port.clear();
		path.clear();
		parameters.clear();
	}

}
