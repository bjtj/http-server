#ifndef __HTTP_RESPONSE_HPP__
#define __HTTP_RESPONSE_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
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
		bool headerSent;
		
		DataTransfer * transfer;
		
	public:
		HttpResponse();
		virtual ~HttpResponse();

		void setStatusCode(int code);
		void setStatusCode(int code, std::string message);
		void setParts(std::vector<std::string> &parts);
		void setContentLength(int length);
		void setContentLength(size_t length);
		void setContentLength(unsigned long long length);
		void setContentType(std::string type);

        bool completeContentTransfer();

		HttpResponseHeader & getHeader();
        void setHeader(HttpHeader & header);
		void setTransfer(DataTransfer * transfer);
		DataTransfer * getTransfer();
		void clearTransfer();
	};
}

#endif
