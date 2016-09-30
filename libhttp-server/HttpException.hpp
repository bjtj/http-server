#ifndef __HTTP_EXCEPTION_HPP__
#define __HTTP_EXCEPTION_HPP__

#include <map>
#include <string>
#include "HttpStatusCodes.hpp"

namespace HTTP {
	
	class HttpException : public OS::Exception {
	private:
		int errorStatusCode;
		std::string errorStatusString;
		std::map<std::string, std::string> headerFields;
		std::string errorDescription;
	public:
		explicit HttpException(int errorStatusCode)
			: errorStatusCode(errorStatusCode),
			  errorStatusString(HttpStatusCodes::getStatusString(errorStatusCode)) {/**/}
		explicit HttpException(int errorStatusCode, const std::string & errorString)
			: errorStatusCode(errorStatusCode), errorString(errorString) {/**/}
		explicit HttpException(int errorStatusCode, const std::string & errorString,
							   const std::string & errorDescription)
			: errorStatusCode(errorStatusCode), errorStatusString(errorStatusString),
			  errorDescription(errorDescription) {/**/}
		explicit HttpException(int errorStatusCode, const std::string & errorString,
							   const std::map<std::string, std::string> & headerFields,
							   const std::string & errorDescription)
			: errorStatusCode(errorStatusCode), errorStatusString(errorStatusString),
			  headerFields(headerFields), errorDescription(errorDescription){/**/}
		virtual ~HttpException() throw() {/**/}
		HttpResponseHeader getHttpResponseHeader() {
			HttpResponseHeader header(errorStatusCode, errorStatusString);
			for (std::map<std::string, std::string>::iterator iter = headerFields.begin();
				 iter != headerFields.end(); iter++) {
				header[iter->first] = iter->second;
			}
			return header;
		}
	};
}

#endif
