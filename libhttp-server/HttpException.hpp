#ifndef __HTTP_EXCEPTION_HPP__
#define __HTTP_EXCEPTION_HPP__

#include <map>
#include <string>
#include "HttpStatusCodes.hpp"

namespace HTTP {

	/**
	 * http exception
	 */
	class HttpException : public OS::Exception {
	private:
		int _statusCode;
		std::string _statusString;
		std::map<std::string, std::string> _headerFields;
		std::string _description;
	public:
		explicit HttpException(int statusCode)
			: _statusCode(statusCode), _statusString(HttpStatusCodes::getStatusString(statusCode))
			{/**/}
		explicit HttpException(int statusCode, const std::string & statusString)
			: _statusCode(statusCode), _statusString(statusString)
			{/**/}
		explicit HttpException(int statusCode, const std::string & statusString, const std::string & description)
			: _statusCode(statusCode), _statusString(statusString), _description(description)
			{/**/}
		explicit HttpException(int statusCode, const std::string & statusString,
							   const std::map<std::string, std::string> & headerFields,
							   const std::string & description)
			: _statusCode(statusCode), _statusString(statusString), _headerFields(headerFields), _description(description)
			{/**/}
		virtual ~HttpException() throw() {/**/}
		
		HttpResponseHeader getHttpResponseHeader() {
			HttpResponseHeader header(_statusCode, _statusString);
			for (std::map<std::string, std::string>::iterator iter = _headerFields.begin(); iter != _headerFields.end(); iter++) {
				header[iter->first] = iter->second;
			}
			return header;
		}

		int & statusCode() { return _statusCode; }
		std::string & statusString() { return _statusString; }
		std::map<std::string, std::string> & headerFields() { return _headerFields; }
		std::string & description() { return _description; }
	};
}

#endif
