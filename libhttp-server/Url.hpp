#ifndef __URL_HPP__
#define __URL_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/StringElements.hpp>
#include <vector>
#include <string>

namespace HTTP {
    
    DECL_NAMED_ONLY_EXCEPTION(UrlParseException);
    
    /**
     * @brief Url
     */
	class Url {
	private:
		std::string username;
		std::string password;
		std::string scheme;
		std::string host;
		std::string port;
		std::string path;
        UTIL::LinkedStringMap parameters;
		std::vector<std::string> knownSchemes;

	public:
        Url();
		Url(const char * urlStr);
		Url(const std::string & urlStr);
		virtual ~Url();

		void initKnownSchemes();

		std::string getUsername() const;
		std::string getPassword() const;
		std::string getScheme() const;
		std::string getProtocol() const;
		std::string getHost() const;
		std::string getPort() const;
		int getIntegerPort() const;
		std::string getPath() const;
        std::string getPathAndQuery() const;
        std::string getPathWithoutPrefix(const std::string & prefix);
        std::string getQueryString() const;
		void getUsername(const std::string & username);
		void getPassword(const std::string & password);
		void setScheme(const std::string & scheme);
		void setProtocol(const std::string & scheme);
		void setHost(const std::string & host);
		void setPort(const std::string & port);
		void setPath(const std::string & path);
        void setRelativePath(const std::string & relativePath);
		Url relativePath(const std::string & relativePath) const;

		void setParameter(const std::string & name, const std::string & value);
        UTIL::LinkedStringMap & getParameters();

        void setUrl(const std::string & urlStr);
		void parseUrlString(const std::string & urlStr);
		void parseAddress(const std::string & address);
		void parsePath(const std::string & resource);
        void parseQuery(const std::string & query);

		std::string getAddress() const;

		int getKnownPort(const std::string & scheme);
		bool isKnownScheme(const std::string & scheme);
        
		virtual std::string toString() const;

		Url& operator=(const std::string & urlStr);
		Url& operator=(const char * urlStr);

	protected:
		void clear();
	};
}

#endif
