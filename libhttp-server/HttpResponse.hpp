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
        /*OS::Socket & socket;*/
		bool complete;
		std::string content;
		bool headerSent;
		int contentLength;
        ReadCounter contentTranferCounter;

		DataTransfer * transfer;
		
	public:
		HttpResponse(/*OS::Socket & socket*/);
		virtual ~HttpResponse();

		void setStatusCode(int code);
		void setStatusCode(int code, std::string message);
		void setParts(std::vector<std::string> &parts);
		void setContentLength(int length);
		void setContentType(std::string type);

		void clearBuffer();
		int send(OS::Socket & socket, const char * buf, int size);
		int write(const std::string & content);
		int write(const char * buf, int size);
		void sendHeaderOnce(OS::Socket & socket);
		bool hasHeaderSent();
		void sendContent(OS::Socket & socket);
		void setComplete();
		bool hasComplete();
        
        bool completeContentTransfer();

		HttpResponseHeader & getHeader();
		void setTransfer(DataTransfer * transfer);
		DataTransfer * getTransfer();
	};
}

#endif
