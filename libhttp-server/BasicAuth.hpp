#ifndef __BASIC_AUTH_HPP__
#define __BASIC_AUTH_HPP__

#include <liboslayer/AutoRef.hpp>
#include <string>
#include "HttpAuth.hpp"

namespace HTTP {

	/**
	 * @brief 
	 */
	class OnBasicAuth {
	public:
		OnBasicAuth();
		virtual ~OnBasicAuth();
		virtual bool onAuth(const std::string & username, const std::string & password) = 0;
	};


	/**
	 * @brief 
	 */
	class BasicAuth : public HttpAuth {
	private:
		OS::AutoRef<OnBasicAuth> onAuth;
	public:
		BasicAuth(OS::AutoRef<OnBasicAuth> onAuth);
		BasicAuth(const std::string & realm, OS::AutoRef<OnBasicAuth> onAuth);
		BasicAuth(const std::string & username, const std::string & password);
		BasicAuth(const std::string & realm, const std::string & username, const std::string & password);
		virtual ~BasicAuth();
		virtual bool validate(HttpRequest & request);
		virtual void setAuthentication(HttpResponse & response);
		bool nil();
	};
}

#endif
