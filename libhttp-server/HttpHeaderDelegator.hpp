#ifndef __HTTP_HEADER_DELEGATOR_HPP__
#define __HTTP_HEADER_DELEGATOR_HPP__

#include "HttpHeader.hpp"
#include <string>
#include <map>
#include <vector>
#include <liboslayer/StringElements.hpp>

namespace http {

	/**
	 * http header delegator
	 */
	class HttpHeaderDelegator : public HttpHeader {
	private:
		HttpHeader & _header;
	public:
		HttpHeaderDelegator(HttpHeader & header);
		virtual ~HttpHeaderDelegator();
		virtual void clear();
		virtual void setParts(const std::vector<std::string> & parts);
		virtual void setParts(const std::string & part1, const std::string & part2, const std::string & part3);
		virtual std::string getPart1() const;
		virtual std::string getPart2() const;
		virtual std::string getPart3() const;
		virtual void setPart1(const std::string & part);
		virtual void setPart2(const std::string & part);
		virtual void setPart3(const std::string & part);
		virtual bool hasHeaderFieldCase(const std::string & name) const;
		virtual std::string getHeaderFieldCase(const std::string & name) const;
		virtual bool hasHeaderField(const std::string & name) const;
		virtual std::string getHeaderField(const std::string & name) const;
		virtual int getHeaderFieldCaseAsInteger(const std::string & name) const;
		virtual int getHeaderFieldAsInteger(std::string name) const;
		virtual void setHeaderField(const std::string & name, const std::string & value);
		virtual void setHeaderField(const std::string & name, const osl::StringList & value);
		virtual void setHeaderField(const std::string & name, const std::vector<std::string> & value);
		virtual void setHeaderFields(const std::map<std::string, std::string> & fields);
		virtual void appendHeaderField(const std::string & name, const std::string & value);
		virtual void appendHeaderFields(const osl::LinkedStringMap & fields);
		virtual void appendHeaderFields(const std::map<std::string, std::string> & fields);
		virtual osl::StringList getHeaderFields(const std::string & name);
		virtual osl::LinkedStringListMap getHeaderFields();
		virtual std::map<std::string, std::string> getHeaderFieldsStdMap();
		virtual void removeHeaderFieldCase(const std::string & name);
		virtual void removeHeaderField(const std::string & name);
		virtual void removeHeaderFieldsCase(const std::string & name);
		virtual void removeHeaderFields(const std::string & name);

        /* HTTP */
		virtual std::string getContentType() const;
		virtual void setContentType(std::string contentType);
		virtual int getContentLength() const;
		virtual void setContentLength(long long contentLength);
		virtual bool isChunkedTransfer() const;
		virtual void setChunkedTransfer(bool chunked);
        virtual void setConnection(const std::string & connection);
        virtual bool keepConnection();
		virtual std::string toString() const;
	};
}

#endif
