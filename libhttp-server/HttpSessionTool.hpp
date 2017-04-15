#ifndef __HTTP_SESSION_TOOL_HPP__
#define __HTTP_SESSION_TOOL_HPP__

#include <liboslayer/Text.hpp>
#include <string>
#include "HttpSessionManager.hpp"

namespace HTTP {
	
	class HttpSessionTool {
	private:
	public:
		HttpSessionTool() {}
		virtual ~HttpSessionTool() {}

		static std::string getSessionId(HttpRequest & request) {
			return request.getParameter("sessionId");
		}

        static OS::AutoRef<HttpSession> getSession(HttpRequest & request, HttpSessionManager & sessionManager) {
			std::string sessionId = getSessionId(request);
            OS::AutoRef<HttpSession> session = (sessionId.empty() ||
									 !sessionManager.hasSession(UTIL::Text::toInt(sessionId))) ?
				sessionManager.createSession() : sessionManager.getSession(UTIL::Text::toInt(sessionId));
		
			if (session->outdated()) {
				sessionManager.destroySession(session->getId());
				return sessionManager.createSession();
			}
			return session;
		}

        static std::string urlMan(const std::string & u, OS::AutoRef<HttpSession> session) {
			size_t f = u.find("?");
			std::string path = (f == std::string::npos) ? u : u.substr(0, f);
			std::string rest = (f == std::string::npos) ? "" : u.substr(f);
			return path + ";sessionId=" + UTIL::Text::toString(session->getId()) + rest;
		}
	};
	
}

#endif
