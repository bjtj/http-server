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

		std::string getMethod() const;
		std::string getPath() const;
		std::string & getHeaderField(const std::string & name);
		std::string getHeaderField(const std::string & name) const;
		std::string & getHeaderFieldIgnoreCase(const std::string & name);
		std::string getHeaderFieldIgnoreCase(const std::string & name) const;
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
