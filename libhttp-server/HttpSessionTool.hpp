#ifndef __HTTP_SESSION_TOOL_HPP__
#define __HTTP_SESSION_TOOL_HPP__

#include <liboslayer/Text.hpp>
#include <liboslayer/Date.hpp>
#include <string>
#include "HttpSessionManager.hpp"

namespace HTTP {
	
	class HttpSessionTool {
	private:
	public:
		HttpSessionTool() {}
		virtual ~HttpSessionTool() {}

		static std::string getSessionId(HttpRequest & request) {
			std::string sessionId = request.getCookie("sessionId");
			if (sessionId.empty() == false) {
				return sessionId;
			}
			return request.getParameter("sessionId");
		}

        static OS::AutoRef<HttpSession> handleSession(HttpRequest & request, HttpResponse & response, HttpSessionManager & sessionManager) {
			std::string sessionId = getSessionId(request);
            OS::AutoRef<HttpSession> session = (sessionId.empty() || !sessionManager.hasSession(sessionId)) ?
				createSession(request, response, sessionManager) : sessionManager.getSession(sessionId);
			if (session->outdated()) {
				sessionManager.destroySession(session->id());
				return createSession(request, response, sessionManager);
			}
			return session;
		}

		static OS::AutoRef<HttpSession> createSession(HttpRequest & request, HttpResponse & response, HttpSessionManager & sessionManager) {
			OS::AutoRef<HttpSession> session = sessionManager.createSession();
			OS::osl_time_t y = {(360ULL * 24ULL * 60ULL * 60ULL), 0};
			OS::Date year(y);
			std::string date = OS::Date::formatRfc1123(OS::Date::now() + year);
			Cookie cookie("sessionId=" + session->id() + "; expires=" + date + "; path=" + request.getDirectory() + "; HttpOnly");
			response.header().removeHeaderFields("Set-Cookie");
			response.setCookie(cookie);
			return session;
		}

        static std::string urlMan(HttpRequest & request, const std::string & u, OS::AutoRef<HttpSession> session) {
			if (request.getCookie("sessionId") == session->id()) {
				return u;
			}
			size_t f = u.find("?");
			std::string path = (f == std::string::npos) ? u : u.substr(0, f);
			std::string rest = (f == std::string::npos) ? "" : u.substr(f);
			return path + ";sessionId=" + session->id() + rest;
		}
	};
	
}

#endif
