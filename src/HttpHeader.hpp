#ifndef __HTTP_HPP__
#define __HTTP_HPP__

#include <string>
#include <utility>
#include <vector>

namespace HTTP {

	class HeaderField {
	private:
		std::string name;
		std::vector<std::string> values;
	public:
		HeaderField(std::string name);
		virtual ~HeaderField();

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

	
	class Header {
	private:
		std::string firstline;
		std::vector<HeaderField> fields;
	public:
		Header();
		virtual ~Header();

		virtual bool contains();
		virtual std::string getParameter(std::string name);
		virtual std::vector<std::string> getParameters(std::string name);
	};

	class HeaderParseResult {
	private:
		bool success;
		int errorCode;
		std::string errorMessage;
        Header header;
	public:
		HeaderParseResult();
		virtual ~HeaderParseResult();

		Header & getHeader();
		bool isSucceeded();
		bool isFailed();
		int getErrorCode();
		std::string getErrorMessage();
	};

	
	class HeaderParser {
	public:
		HeaderParser();
		virtual ~HeaderParser();

		HeaderParseResult parse(std::string & header);
	};

}

#endif
