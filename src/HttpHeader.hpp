#ifndef __HTTP_HPP__
#define __HTTP_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

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

		bool isValid();
		
		void setFirstLine(std::string & firstline);
		void setParts(std::vector<std::string> & parts);
		std::string getPart1();
		std::string getPart2();
		std::string getPart3();
		
		std::string getHeaderField(std::string name);
		void setHeaderField(std::string name, std::string value);
		
		std::string getParameter(std::string name);
		std::vector<std::string> getParameters(std::string name);
		void setParameter(std::string name, std::string value);

		virtual std::string toString();
	};

	class HttpHeaderWrapper : public HttpHeader {
	private:
        HttpHeader & header;
	public:
		HttpHeaderWrapper(HttpHeader & header) : header(header) {}
		virtual ~HttpHeaderWrapper() {}

		HttpHeader & getHeader() {return header;}
	};

	
	/**
	 * @brief http request header
	 */
	class HttpRequestHeader : public HttpHeaderWrapper {
	private:
	public:
		HttpRequestHeader(HttpHeader & header) : HttpHeaderWrapper(header) {}
		virtual ~HttpRequestHeader() {}

		std::string getMethod() {return getHeader().getPart1();}
		std::string getPath() {return getHeader().getPart2();}
		std::string getProtocol() {return getHeader().getPart3();}
	};

	/**
	 * @brief http request header
	 */
	class HttpResponseHeader : public HttpHeaderWrapper {
	private:
	public:
		HttpResponseHeader(HttpHeader & header) : HttpHeaderWrapper(header) {}
		virtual ~HttpResponseHeader() {}

		std::string getProtocol() {return getHeader().getPart1();}
		int getStatus() {return atoi(getHeader().getPart2().c_str());}
		std::string getMessage() {return getHeader().getPart3();}
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

		int parseFirstLine(HttpHeader & header, std::string & line);
		int parseParam(HttpHeader & header, std::string line);
		int parse(std::string & header);
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
