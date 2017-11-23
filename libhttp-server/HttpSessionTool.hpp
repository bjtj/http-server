#ifndef __HTTP_SESSION_TOOL_HPP__
#define __HTTP_SESSION_TOOL_HPP__

#include <liboslayer/Text.hpp>
#include <liboslayer/Date.hpp>
#include <string>
#include "HttpSessionManager.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace HTTP {
	
	class HttpSessionTool {
	private:
		static const std::string KEY_SESSION_ID;
		OS::osl_time_t _expire;
		std::string _path;
	public:
		HttpSessionTool();
		virtual ~HttpSessionTool();
		bool isCookieSession(HttpRequest & request);
		std::string getSessionId(HttpRequest & request);
		std::string cookiePath(const std::string & path);
		void setCookieSession(HttpRequest & request,
									 HttpResponse & response,
									 OS::AutoRef<HttpSession> & session);
        OS::AutoRef<HttpSession> handleSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
		OS::AutoRef<HttpSession> createSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
        static std::string url(HttpRequest & request,
							   const std::string & u,
							   OS::AutoRef<HttpSession> session);
		OS::osl_time_t & expire();
		std::string & path();
	};
}

#endif
