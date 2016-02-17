#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

#include "Url.hpp"

namespace HTTP {

	using namespace std;
    using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();

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
        return Text::toString(parameters.toNameValueList(), "=", "&");
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

	Url Url::relativePath(const string & relativePath) {
		Url u = *this;
		u.setRelativePath(relativePath);
		return u;
	}

	void Url::setParameter(const string & name, const string & value) {
        parameters.append(name, value);
	}
	
	StringListMap & Url::getParameters() {
		return parameters;
	}
    
    void Url::setUrl(const string & urlStr) {
        parseUrlString(urlStr);
    }

	void Url::parseUrlString(string urlStr) {

		clear();
		
		size_t f = urlStr.find("://");
		if (f == string::npos) {
			throw UrlParseException("no protocol found", -1, 0);
		}
		
		scheme = urlStr.substr(0, f);
		urlStr = urlStr.substr(f + 3);
		f = urlStr.find("/");
		if (f != string::npos) {
			vector<string> addr = parseAddress(urlStr.substr(0, f));
			setAddress(addr);
			parsePath(urlStr.substr(f));
		} else {
			vector<string> addr = parseAddress(urlStr);
			setAddress(addr);
			path = "/";
		}
	}

	vector<string> Url::parseAddress(const string & address) {
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
    
	string Url::toString() {
        
        string p = getPathAndQuery();
        if (Text::startsWith(p, "/")) {
            p = p.substr(1);
        }

		return scheme + "://" + host + ":" + port + "/" + p;
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
