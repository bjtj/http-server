#ifndef __HTTP_HPP__
#define __HTTP_HPP__

#include <string>
#include <utility>
#include <vector>

namespace HTTP {

	class HttpHeaderField {
	private:
		std::string name;
		std::vector<std::string> values;
	public:
		HttpHeaderField(std::string name);
		virtual ~HttpHeaderField();

		virtual bool empty();
		virtual size_t size();
		virtual std::string & getName();
		virtual std::string & getFirstParameter();
		virtual std::vector<std::string> & getParameters();
		virtual std::string & getParameter(int index);
		virtual void setParameter(std::string param);
		virtual void setParameters(std::vector<std::string> & params);
		virtual void appendParamter(std::string param);
		virtual void appendParameters(std::vector<std::string> & param);

		std::string & operator[](int index);
		virtual std::string toString();
	};

	
	class HttpHeader {
	private:
		std::string firstline;
		std::vector<HttpHeaderField> fields;
	public:
		HttpHeader();
		virtual ~HttpHeader();

		virtual bool contains();
		virtual std::string getParameter(std::string name);
		virtual std::vector<std::string> getParameters(std::string name);
	};

	class HttpHeaderParseResult {
	private:
		bool success;
		int errorCode;
		std::string errorMessage;
        HttpHeader header;
	public:
		HttpHeaderParseResult();
		virtual ~HttpHeaderParseResult();

		HttpHeader & getHeader();
		bool isSucceeded();
		bool isFailed();
		int getErrorCode();
		std::string getErrorMessage();
	};

	
	class HttpHeaderParser {
	public:
		HttpHeaderParser();
		virtual ~HttpHeaderParser();

		HttpHeaderParseResult parse(std::string & header);
	};

}

#endif
