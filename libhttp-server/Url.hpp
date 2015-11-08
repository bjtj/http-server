#ifndef __URL_HPP__
#define __URL_HPP__

#include <vector>
#include <string>

namespace HTTP {

	class Url {
	private:
		std::string scheme;
		std::string host;
		std::string port;
		std::string path;
		std::vector<std::pair<std::string, std::string> > params;

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
		void setScheme(const std::string & scheme);
		void setProtocol(const std::string & scheme);
		void setHost(const std::string & host);
		void setPort(const std::string & port);
		void setPath(const std::string & path);
        void setRelativePath(const std::string & relativePath);

		void appendParam(std::string name, std::string value);
		std::vector<std::pair<std::string, std::string> > & getParams();

        void setUrl(const std::string & urlStr);
		void parseUrlString(std::string urlStr);
		std::vector<std::string> parseAddress(std::string address);
		void setAddress(std::vector<std::string> & addr);

		std::string getAddress() const;

        std::string enoughPath(const std::string & path);
		virtual std::string toString();

		Url& operator=(const std::string & urlStr);
		Url& operator=(const char * urlStr);

	protected:
		void clear();
	};
}

#endif
