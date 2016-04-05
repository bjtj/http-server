#ifndef __HTTP_HEADER_HPP__
#define __HTTP_HEADER_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

#include <liboslayer/StringElement.hpp>
#include "HttpParameter.hpp"

namespace HTTP {

	/**
	 * @breif HttpHeader
	 */
	class HttpHeader {
	private:
		bool valid;
		std::string firstline;
		std::string part1;
		std::string part2;
		std::string part3;
		UTIL::LinkedStringMap fields;
		
	public:
		HttpHeader();
		HttpHeader(std::string par1, std::string part2, std::string part3);
		virtual ~HttpHeader();

		virtual bool isValid();
		virtual void clear();
		virtual void setHeader(const HttpHeader & other);
		virtual void setFirstLine(std::string & firstline);
		virtual void setParts(std::vector<std::string> & parts);
		virtual void setParts(std::string par1, std::string part2, std::string part3);
		virtual std::string getPart1() const;
		virtual std::string getPart2() const;
		virtual std::string getPart3() const;
		virtual void setPart1(const std::string & part);
		virtual void setPart2(const std::string & part);
		virtual void setPart3(const std::string & part);
		std::string makeFirstLine() const;
		bool hasHeaderField(const std::string & name) const;
		std::string & getHeaderField(const std::string & name);
		std::string getHeaderField(const std::string & name) const;
		bool hasHeaderFieldIgnoreCase(const std::string & name) const;
		std::string & getHeaderFieldIgnoreCase(const std::string & name);
		std::string getHeaderFieldIgnoreCase(const std::string & name) const;
		int getHeaderFieldAsInteger(std::string name) const;
		int getHeaderFieldIgnoreCaseAsInteger(std::string name) const;
		void setHeaderField(std::string name, std::string value);
		void setHeaderFields(std::map<std::string, std::string> & fields);
		void appendHeaderFields(const UTIL::LinkedStringMap & fields);
		void appendHeaderFields(const std::map<std::string, std::string> & fields);
		UTIL::LinkedStringMap & getHeaderFields();
		std::map<std::string, std::string> getHeaderFieldsStdMap();
		void removeHeaderField(const std::string & name);
		void removeHeaderFieldIgnoreCase(const std::string & name);

        /* HTTP */
        
		std::string getContentType() const;
		void setContentType(std::string contentType);
		int getContentLength() const;
		void setContentLength(int contentLength);
		void setContentLength(size_t contentLength);
		void setContentLength(unsigned long long contentLength);
		bool isChunkedTransfer() const;
		void setChunkedTransfer(bool chunked);
        void setConnection(const std::string & connection);
        bool keepConnection();
		virtual std::string toString() const;
		std::string & operator[] (const std::string & headerFieldName);
	};


	/**
	 * @breif HttpRequestHeader
	 */
	class HttpRequestHeader : public HttpHeader {
	private:
		std::string resourcePath;
		std::map<std::string, HttpParameter> params;
		std::string fragment;

	public:
		HttpRequestHeader();
		HttpRequestHeader(const HttpHeader & other);
		virtual ~HttpRequestHeader();

		virtual void clear();
        virtual void setHeader(const HttpHeader & other);
		std::string getMethod() const;
		void setMethod(const std::string & method) ;
		std::string getPath() const;
		void setPath(const std::string & path) ;
		std::string getProtocol() const;
		void setProtocol(const std::string & protocol) ;
		std::string extractResourcePath(const std::string & path);
		std::string extractWithoutSemicolon(const std::string & path);
		std::string extractWithoutQuery(const std::string & path);
		std::string extractQuery(const std::string & path);
		std::string extractWithoutFragment(const std::string & path);
		std::string extractFragment(const std::string & path);
		std::vector<UTIL::NameValue> parseSemiColonParameters(const std::string & path);
		UTIL::NameValue parseNameValue(const std::string & text);
		void parsePath(const std::string & path);
		void parseQuery(const std::string & query);
        std::vector<std::string> getParameterNames();
		std::string getParameter(std::string name);
		std::vector<std::string> getParameters(std::string name);
		void setParameter(std::string name, std::string value);
		void setParameters(std::vector<UTIL::NameValue> & nvs);
        void setHost(const std::string & host);
	};

	/**
	 * @breif HttpResponseHeader
	 */
	class HttpResponseHeader : public HttpHeader {
	private:
	public:
		HttpResponseHeader();
		HttpResponseHeader(const HttpHeader & other);
		virtual ~HttpResponseHeader();

		std::string getProtocol() const;
		void setProtocol(const std::string & protocol);
		int getStatusCode() const;
		void setStatusCode(int statusCode);
		std::string getMessage() const;
		void setMessage(const std::string & message);
        
        bool isRedirection();
        std::string getRedirectionLocation();
	};
}

#endif
