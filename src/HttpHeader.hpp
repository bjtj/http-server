#ifndef __HTTP_HPP__
#define __HTTP_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>
#include "Text.hpp"

namespace HTTP {

	/**
	 * @brief http header field
	 */
	class HttpParameter {
	private:
		std::string name;
		std::vector<std::string> values;
	public:
		HttpParameter();
		HttpParameter(std::string name);
		HttpParameter(std::string name, std::string value);
		virtual ~HttpParameter();
		
		bool empty();
		size_t size();
		std::string & getName();
		void setName(std::string name);
		std::string getFirstValue();
		std::vector<std::string> & getValues();
		std::string & getValue(int index);
		void setValue(std::string value);
		void setValues(std::vector<std::string> & values);
		void appendValue(std::string value);
		void appendValues(std::vector<std::string> & values);

		std::string & operator[](int index);
		virtual std::string toString();
	};

	/**
	 * @breif http header
	 */
	class HttpHeader {
	private:
		bool valid;
		std::string firstline;
		std::vector<std::string> parts;
		std::map<std::string, std::string> fields;
		std::map<std::string, HttpParameter> params;
	public:
		HttpHeader();
		virtual ~HttpHeader();

		virtual bool isValid();
		
		virtual void setFirstLine(std::string & firstline);
		virtual void setParts(std::vector<std::string> & parts);
		virtual std::string getPart1();
		virtual std::string getPart2();
		virtual std::string getPart3();
		virtual void setPart1(std::string part);
		virtual void setPart2(std::string part);
		virtual void setPart3(std::string part);
		
		virtual std::string getHeaderField(std::string name);
		virtual void setHeaderField(std::string name, std::string value);
		
		virtual std::string getParameter(std::string name);
		virtual std::vector<std::string> getParameters(std::string name);
		virtual void setParameter(std::string name, std::string value);

		virtual std::string toString();
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

	/**
	 * @brief http header parse result
	 */
	class HttpHeaderParseResult {
	private:
		bool success;
		int errorCode;
		std::string errorMessage;
        HttpHeader header;
	public:
		HttpHeaderParseResult();
		HttpHeaderParseResult(bool success, int errorCode, std::string errorMessage);
		virtual ~HttpHeaderParseResult();

		int setResult(bool success, int errorCode, std::string errorMessage);
		HttpHeader & getHeader();
		bool succeeded();
		int getErrorCode();
		std::string getErrorMessage();
	};

	/**
	 * @brief http header parser
	 */
	class HttpHeaderParser {
	private:
        HttpHeaderParseResult result;
	public:
		HttpHeaderParser();
		virtual ~HttpHeaderParser();

		static size_t parseParam(HttpHeader & header, const std::string & param, size_t & f);
		static void parseParams(HttpHeader & header, std::string params, size_t offset);

		int parseFirstLine(HttpHeader & header, std::string & line);
		int parseHeaderField(HttpHeader & header, std::string line);
		std::string readLine(const std::string & full, size_t & f);
		int parse(const std::string & header);
		HttpHeaderParseResult & getResult();
		HttpHeader & getHeader();
	};

	/**
	 * @brief http header reader
	 */
	class HttpHeaderReader {
	private:
		HttpHeaderParser parser;
		std::string buffer;
	public:
		HttpHeaderReader();
		virtual ~HttpHeaderReader();

		bool complete();
		int cutEndPos();
		void append(char * data, int size);
		bool parse();
		int read(char * data, int size);
		HttpHeader & getHeader();
	};

}

#endif
