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

		std::string getScheme();
		std::string getProtocol();
		std::string getHost();
		std::string getPort();
		int getIntegerPort();
		std::string getPath();
		void setScheme(std::string scheme);
		void setProtocol(std::string scheme);
		void setHost(std::string host);
		void setPort(std::string port);
		void setPath(std::string path);

		void appendParam(std::string name, std::string value);
		std::vector<std::pair<std::string, std::string> > & getParams();

		void parseUrlString(std::string urlStr);
		std::vector<std::string> parseAddress(std::string address);
		void setAddress(std::vector<std::string> & addr);

		std::string getAddress();

		virtual std::string toString();

		Url& operator=(const std::string & urlStr);
		Url& operator=(const char * urlStr);

	protected:
		void clear();
	};
}

#endif
