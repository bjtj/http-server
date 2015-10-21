#ifndef __HTTP_HEADER_HPP__
#define __HTTP_HEADER_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

#include "HttpParameter.hpp"

namespace HTTP {

	/**
	 * @breif http header
	 */
	class HttpHeader {
	private:
		bool valid;
		std::string firstline;
		std::string part1;
		std::string part2;
		std::string part3;
		std::map<std::string, std::string> fields;
		std::map<std::string, HttpParameter> params;
		
	public:
		HttpHeader();
		HttpHeader(std::string par1, std::string part2, std::string part3);
		virtual ~HttpHeader();

		virtual bool isValid();
		
		virtual void setFirstLine(std::string & firstline);
		virtual void setParts(std::vector<std::string> & parts);
		virtual void setParts(std::string par1, std::string part2, std::string part3);
		virtual std::string getPart1();
		virtual std::string getPart2();
		virtual std::string getPart3();
		virtual void setPart1(std::string part);
		virtual void setPart2(std::string part);
		virtual void setPart3(std::string part);

		std::string makeFirstLine();
		
		virtual std::string getHeaderField(std::string name);
		virtual std::string getHeaderFieldIgnoreCase(std::string name);
		virtual int getHeaderFieldAsInteger(std::string name);
		virtual int getHeaderFieldIgnoreCaseAsInteger(std::string name);
		virtual void setHeaderField(std::string name, std::string value);
		virtual void setHeaderFields(std::map<std::string, std::string> & fields);
		virtual void appendHeaderFields(std::map<std::string, std::string> & fields);
		virtual std::map<std::string, std::string> & getHeaderFields();
		virtual void removeHeaderField(std::string name);
		virtual void removeHeaderFieldIgnoreCase(std::string name);

		std::string getContentType();
		void setContentType(std::string contentType);
		int getContentLength();
		void setContentLength(int contentLength);
		bool isChunkedTransfer();
		void setChunkedTransfer(bool chunked);
		
		virtual std::string getParameter(std::string name);
		virtual std::vector<std::string> getParameters(std::string name);
		virtual void setParameter(std::string name, std::string value);

		virtual std::string toString();

		std::string operator[] (const std::string & headerFieldName);
	};

	/**
	 * @brief http header wrapper
	 */
	class HttpHeaderWrapper : public HttpHeader {
	private:
        HttpHeader & header;
	public:
		HttpHeaderWrapper(HttpHeader & header) : header(header) {}
		virtual ~HttpHeaderWrapper() {}

		HttpHeader & getHeader() {return header;}

		virtual bool isValid() {return getHeader().isValid();}
		
		virtual void setFirstLine(std::string & firstline) {getHeader().setFirstLine(firstline);}
		virtual void setParts(std::vector<std::string> & parts) {getHeader().setParts(parts);}
		virtual std::string getPart1() {return getHeader().getPart1();}
		virtual std::string getPart2() {return getHeader().getPart2();}
		virtual std::string getPart3() {return getHeader().getPart3();}
		virtual void setPart1(std::string part) {getHeader().setPart1(part);}
		virtual void setPart2(std::string part) {getHeader().setPart2(part);}
		virtual void setPart3(std::string part) {getHeader().setPart3(part);}
		
		virtual std::string getHeaderField(std::string name) {return getHeader().getHeaderField(name);}
		virtual std::string getHeaderFieldIgnoreCase(std::string name) {
			return getHeader().getHeaderFieldIgnoreCase(name);}
		virtual void setHeaderField(std::string name, std::string value) {
			getHeader().setHeaderField(name, value);}
		
		virtual std::string getParameter(std::string name) {return getHeader().getParameter(name);}
		virtual std::vector<std::string> getParameters(std::string name) {
			return getHeader().getParameters(name);}
		virtual void setParameter(std::string name, std::string value) {
			getHeader().setParameter(name, value);}
		
		virtual std::string toString() {return header.toString();}
	};

	
	/**
	 * @brief http request header
	 */
	class HttpRequestHeader : public HttpHeaderWrapper {
	private:
		bool parsed;
	public:
		HttpRequestHeader(HttpHeader & header);
		virtual ~HttpRequestHeader();

		void parsePath();
		void parseParams(const std::string & params, size_t offset);

		static size_t parseParam(HttpHeader & header, const std::string & param, size_t & f);
		static void parseParams(HttpHeader & header, std::string params, size_t offset);

		virtual void setPart2(std::string part);

		std::string getMethod();
		std::string getPath();
		std::string getProtocol();
	};

	/**
	 * @brief http request header
	 */
	class HttpResponseHeader : public HttpHeaderWrapper {
	private:
		HttpHeader header;
	public:
		HttpResponseHeader();
		HttpResponseHeader(HttpHeader & header);
		virtual ~HttpResponseHeader();

		std::string getProtocol();
		int getStatusCode();
		void setStatusCode(int status);
		std::string getMessage();
		void setMessage(std::string message);
	};

}

#endif
