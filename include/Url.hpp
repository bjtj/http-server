#ifndef __URL_HPP__
#define __URL_HPP__

#include <vector>
#include <string>

namespace URL {

	class Url {
	private:
		std::string scheme;
		std::string host;
		std::string port;
		std::string path;
		std::vector<std::pair<std::string, std::string> > params;
		
	public:
		Url();
		virtual ~Url();

		std::string getScheme();
		std::string getHost();
		std::string getPort();
		std::string getPath();
		void setScheme(std::string scheme);
		void setHost(std::string host);
		void setPort(std::string port);
		void setPath(std::string path);

		void appendParam(std::string name, std::string value);
		std::vector<std::pair<std::string, std::string> > & getParams();

		virtual std::string toString();
	};


	class UrlParser
	{
	public:
		UrlParser();
		virtual ~UrlParser();

		virtual Url parse(std::string url);
	};

	
}

#endif
