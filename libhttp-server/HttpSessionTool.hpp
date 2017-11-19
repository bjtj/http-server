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
	public:
		HttpSessionTool();
		virtual ~HttpSessionTool();
		static bool isCookieSession(HttpRequest & request);
		static std::string getSessionId(HttpRequest & request);
		static std::string cookiePath(const std::string & path);
		static void setCookieSession(HttpRequest & request,
									 HttpResponse & response,
									 OS::AutoRef<HttpSession> & session);
        static OS::AutoRef<HttpSession> handleSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
		static OS::AutoRef<HttpSession> createSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
        static std::string url(HttpRequest & request,
							   const std::string & u,
							   OS::AutoRef<HttpSession> session);
	};
}

#endif
