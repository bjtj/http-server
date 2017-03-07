#ifndef __WEB_SERVER_UTIL_HPP__
#define __WEB_SERVER_UTIL_HPP__

#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "DataTransfer.hpp"
#include <string>
#include <liboslayer/File.hpp>

namespace HTTP {

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
		bool parseRange(const std::string & range, size_t & from, size_t & to);
	};
}

#endif
