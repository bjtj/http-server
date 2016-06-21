#ifndef __BASIC_AUTH_HPP__
#define __BASIC_AUTH_HPP__

#include <string>
#include "HttpAuth.hpp"

namespace HTTP {
	
	class BasicAuth : public HttpAuth {
	private:
		std::string _username;
		std::string _password;
	public:
		BasicAuth(const std::string & username, const std::string & password);
		virtual ~BasicAuth();
		virtual bool validate(HttpRequest & request);
		virtual void setAuthentication(HttpResponse & response);
		std::string & username();
		std::string & password();
	};
}

#endif
