#ifndef __URL_HPP__
#define __URL_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/StringElement.hpp>
#include <vector>
#include <string>

namespace HTTP {
    
    DECL_NAMED_ONLY_EXCEPTION(UrlParseException);
    
    /**
     * @brief Url
     */
	class Url {
	private:
		std::string scheme;
		std::string host;
		std::string port;
		std::string path;
        UTIL::StringListMap parameters;

	public:
        Url();
		Url(const char * urlStr);
		Url(const std::string & urlStr);
		virtual ~Url();

		std::string getScheme() const;
		std::string getProtocol() const;
		std::string getHost() const;
		std::string getPort() const;
		int getIntegerPort() const;
		std::string getPath() const;
        std::string getPathAndQuery() const;
        std::string getPathWithoutPrefix(const std::string & prefix);
        std::string getQueryString() const;
		void setScheme(const std::string & scheme);
		void setProtocol(const std::string & scheme);
		void setHost(const std::string & host);
		void setPort(const std::string & port);
		void setPath(const std::string & path);
        void setRelativePath(const std::string & relativePath);

		void setParameter(const std::string & name, const std::string & value);
        UTIL::StringListMap & getParameters();

        void setUrl(const std::string & urlStr);
		void parseUrlString(std::string urlStr);
		std::vector<std::string> parseAddress(const std::string & address);
		void setAddress(std::vector<std::string> & addr);
        void parseQuery(const std::string & query);

		std::string getAddress() const;

        
		virtual std::string toString();

		Url& operator=(const std::string & urlStr);
		Url& operator=(const char * urlStr);

	protected:
		void clear();
	};
}

#endif
