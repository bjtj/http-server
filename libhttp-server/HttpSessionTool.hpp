#ifndef __HTTP_SESSION_TOOL_HPP__
#define __HTTP_SESSION_TOOL_HPP__

#include <liboslayer/Text.hpp>
#include <liboslayer/Date.hpp>
#include <string>
#include "HttpSessionManager.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace http {
	
	class HttpSessionTool {
	private:
		static const std::string KEY_SESSION_ID;
		osl::osl_time_t _expire;
		std::string _path;
	public:
		HttpSessionTool();
		virtual ~HttpSessionTool();
		bool isCookieSession(HttpRequest & request);
		std::string getSessionId(HttpRequest & request);
		std::string cookiePath(const std::string & path);
		void setCookieSession(HttpRequest & request,
									 HttpResponse & response,
									 osl::AutoRef<HttpSession> & session);
        osl::AutoRef<HttpSession> handleSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
		osl::AutoRef<HttpSession> createSession(HttpRequest & request,
													  HttpResponse & response,
													  HttpSessionManager & sessionManager);
        static std::string url(HttpRequest & request,
							   const std::string & u,
							   osl::AutoRef<HttpSession> session);
		osl::osl_time_t & expire();
		std::string & path();
	};
}

#endif
