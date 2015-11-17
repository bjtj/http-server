#ifndef __HTTP_REQUEST_HPP__
#define __HTTP_REQUEST_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
#include "HttpHeader.hpp"
#include "ChunkedReader.hpp"
#include "Packet.hpp"
//#include "MultiConn.hpp"

#include "DataTransfer.hpp"

namespace HTTP {

	/**
	 * @brief http request
	 */
	class HttpRequest {
	private:
        HttpRequestHeader header;
		std::string content;
        ChunkedBuffer chunkedBuffer;
        std::string stringBuffer;

		Packet * contentPacket;
		ReadCounter contentReadCounter;

		DataTransfer * transfer;

	public:
        HttpRequest();
		HttpRequest(HttpHeader & header);
		virtual ~HttpRequest();

        void setHeader(HttpHeader & header);
		std::string getMethod() const;
		std::string getPath() const;
		std::string & getHeaderField(const std::string & name);
		std::string getHeaderField(const std::string & name) const;
		std::string & getHeaderFieldIgnoreCase(const std::string & name);
		std::string getHeaderFieldIgnoreCase(const std::string & name) const;
		std::map<std::string, std::string> & getHeaderFields();
        std::vector<std::string> getParameterNames();
		std::string getParameter(const std::string & name);
		std::string getParameter(const char * name);
		std::vector<std::string> getParameters(std::string & name);

		HttpRequestHeader & getHeader();
		const HttpRequestHeader & getHeader() const;
        ChunkedBuffer & getChunkedBuffer();
        std::string & getStringBuffer();
        
		int getContentLength();
		std::string getContentType();

		void setContentPacket(Packet * packet);
		void readChunkedBuffer(ChunkedBuffer & buffer);
		bool completeContentRead();

		DataTransfer * getTransfer();
		void setTransfer(DataTransfer * transfer);
	};

}

#endif
