#ifndef __HTTP_REQUEST_HPP__
#define __HTTP_REQUEST_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
#include "HttpHeader.hpp"

namespace HTTP {

	/**
	 * @brief http request
	 */
	class HttpRequest {
	private:
        HttpRequestHeader header;
		std::string content;
		int contentSize;
		OS::Socket & socket;
	public:
		HttpRequest(HttpHeader & header, OS::Socket & socket);
		virtual ~HttpRequest();

		std::string getMethod();
		std::string getPath();
		std::string getHeaderField(std::string & name);
		std::map<std::string, std::string> & getHeaderFields();
		std::string getParameter(const std::string & name);
		std::string getParameter(const char * name);
		std::vector<std::string> getParameters(std::string & name);

		HttpRequestHeader & getHeader();
		void appendContent(char * buffer, size_t size);
		void compactContent();
		std::string & getContent();

		int getContentLength();
		std::string getContentType();
		bool remaining();
	};

}

#endif
