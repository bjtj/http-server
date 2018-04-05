#ifndef __HTTP_RESPONSE_HPP__
#define __HTTP_RESPONSE_HPP__

#include <vector>
#include <string>
#include <map>

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/File.hpp>
#include "HttpHeader.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"
#include "Cookie.hpp"
#include "HttpRange.hpp"

namespace HTTP {
	

	/**
	 * @brief http response
	 */
	class HttpResponse : public HttpHeaderDelegator {
	private:
		HttpResponseHeader _header;
        OS::AutoRef<DataTransfer> transfer;
		bool _redirectRequested;
		std::string _redirectLocation;
		bool _forwardRequested;
		std::string _forwardLocation;
		std::map<std::string, std::string> _props;
		
	public:
		HttpResponse();
		virtual ~HttpResponse();
        virtual void clear();
		void setStatus(int statusCode);
		void setStatus(int statusCode, const std::string & statusString);
		int getStatusCode();
		void setParts(std::vector<std::string> &parts);
        bool completeContentTransfer();
		HttpResponseHeader & header();
        void setTransfer(OS::AutoRef<DataTransfer> transfer);
        OS::AutoRef<DataTransfer> getTransfer();
		void clearTransfer();
		void setRedirect(const std::string & location);
		void cancelRedirect();
		void setForward(const std::string & location);
		void cancelForward();
		std::string getRedirectLocation();
		std::string getForwardLocation();
		bool redirectRequested();
		bool forwardRequested();
		bool isRedirectionStatus();
		std::string getLocation();
		std::string & operator[] (const std::string & name);
		void appendCookies(const std::vector<Cookie> & cookies);
		void appendCookie(const Cookie & cookie);
		void setFixedTransfer(const std::string & content);
		void setFileTransfer(const std::string & filepath);
		void setFileTransfer(OS::File & file);
		void setPartialFileTransfer(const HttpRange & range, const OS::File & file);
		void setRange(const HttpRange & range, size_t filesize);
	};
}

#endif
