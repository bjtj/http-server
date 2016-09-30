#ifndef __HTTP_STATUS_CODES_HPP__
#define __HTTP_STATUS_CODES_HPP__

#include <string>
#include <map>

namespace HTTP {
	
	class HttpStatusCodes {
	private:
        static std::map<int, std::string> _codes;
	public:
		HttpStatusCodes();
		virtual ~HttpStatusCodes();
		static std::string getStatusString(int code);
		static std::map<int, std::string> & codes();
	};

	
}

#endif
