#ifndef __WEB_SERVER_UTIL_HPP__
#define __WEB_SERVER_UTIL_HPP__

#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "DataTransfer.hpp"
#include <string>
#include <liboslayer/File.hpp>

namespace HTTP {

	/**
	 * 
	 */
	class Range {
	private:
		size_t _from;
		size_t _to;
	public:
		Range();
		Range(size_t from, size_t to);
		virtual ~Range();
		size_t & from();
		size_t & to();
		size_t size() const;
	};


	/**
	 * web server util
	 */
	class WebServerUtil {
	private:
	public:
		WebServerUtil();
		virtual ~WebServerUtil();
		void setFixedTransfer(HttpResponse & response, const std::string & content);
		void setFileTransfer(HttpResponse & response, const std::string & filepath);
		void setFileTransfer(HttpResponse & response, OS::File & file);
		void setPartialFileTransfer(HttpRequest & request, HttpResponse & response, OS::File & file);
		Range parseRange(const std::string & range);
	};
}

#endif
