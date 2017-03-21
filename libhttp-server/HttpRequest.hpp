#ifndef __HTTP_REQUEST_HPP__
#define __HTTP_REQUEST_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include "HttpHeader.hpp"
#include "ChunkedReader.hpp"
#include "Packet.hpp"

#include "DataTransfer.hpp"

namespace HTTP {

	/**
	 * @brief http request
	 */
	class HttpRequest : public HttpHeaderDelegator {
	private:
        HttpRequestHeader _header;
        UTIL::AutoRef<DataTransfer> transfer;
        OS::InetAddress remoteAddress;
		OS::InetAddress localAddress;

	public:
        HttpRequest();
		virtual ~HttpRequest();
		
        virtual void clear();
        void setHeader(const HttpHeader & header);
		std::string getMethod() const;
		std::string getPath() const;
		void setPath(const std::string & path);
		std::string getRawPath() const;
        std::vector<std::string> getParameterNames();
		std::string getParameter(const std::string & name);
		std::string getParameter(const char * name);
		std::vector<std::string> getParameters(std::string & name);
		HttpRequestHeader & header();
		bool isWwwFormUrlEncoded();
		void parseWwwFormUrlencoded();
        UTIL::AutoRef<DataTransfer> getTransfer();
        void setTransfer(UTIL::AutoRef<DataTransfer> transfer);
        void clearTransfer();
        void setRemoteAddress(const OS::InetAddress & remoteAddress);
        OS::InetAddress & getRemoteAddress();
		void setLocalAddress(const OS::InetAddress & localAddress);
		OS::InetAddress & getLocalAddress();
	};

}

#endif
