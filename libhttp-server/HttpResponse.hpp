#ifndef __HTTP_RESPONSE_HPP__
#define __HTTP_RESPONSE_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include "HttpHeader.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

namespace HTTP {

	/**
	 * @brief http response
	 */
	class HttpResponse {
	private:
		HttpResponseHeader header;
        UTIL::AutoRef<DataTransfer> transfer;
		bool _needRedirect;
		std::string redirectLocation;
		std::map<std::string, std::string> props;
		
	public:
		HttpResponse();
		virtual ~HttpResponse();
        
        void clear();
		void setStatus(int statusCode);
		void setStatus(int statusCode, const std::string & statusString);
		int getStatusCode();
		void setParts(std::vector<std::string> &parts);
		void setContentLength(unsigned long long length);
		void setContentType(const std::string & type);
        bool completeContentTransfer();
		std::string getHeaderField(const std::string & name) const;
		std::string getHeaderFieldIgnoreCase(const std::string & name) const;
		HttpResponseHeader & getHeader();
        void setHeader(HttpHeader & header);
        void setTransfer(UTIL::AutoRef<DataTransfer> transfer);
        UTIL::AutoRef<DataTransfer> getTransfer();
		void clearTransfer();
		void setRedirect(const std::string & location);
		std::string getRedirectLocation();
		bool needRedirect();
		std::string & operator[] (const std::string & name);
	};
}

#endif
