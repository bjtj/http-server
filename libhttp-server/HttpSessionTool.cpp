#include "HttpSessionTool.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;

	const string HttpSessionTool::KEY_SESSION_ID = "sessionId";
	
	HttpSessionTool::HttpSessionTool() {
	}
	HttpSessionTool::~HttpSessionTool() {
	}

	bool HttpSessionTool::isCookieSession(HttpRequest & request) {
		string sessionId = request.getCookie(KEY_SESSION_ID);
		if (sessionId.empty() == false) {
			return true;
		}
		return false;
	}

	string HttpSessionTool::getSessionId(HttpRequest & request) {
		string sessionId = request.getCookie(KEY_SESSION_ID);
		if (sessionId.empty() == false) {
			return sessionId;
		}
		return request.getParameter(KEY_SESSION_ID);
	}

	string HttpSessionTool::cookiePath(const string & path) {
		if (path.size() > 1) {
			for (size_t i = path.size() - 1; i > 1; i--) {
				if (path[i] != '/') {
					return path.substr(0, i+1);
				}
			}
		}
		return path;
	}

	void HttpSessionTool::setCookieSession(HttpRequest & request,
										   HttpResponse & response,
										   AutoRef<HttpSession> & session) {
		osl_time_t y = {(360ULL * 24ULL * 60ULL * 60ULL), 0};
		Date year(y);
		string date = Date::formatRfc1123(Date::now() + year);
		Cookie cookie(KEY_SESSION_ID + "=" + session->id() +
					  "; expires=" + date +
					  "; path=" + cookiePath(request.getDirectory()) +
					  "; HttpOnly");
		response.removeHeaderFields("Set-Cookie");
		response.appendCookie(cookie);
	}

	AutoRef<HttpSession> HttpSessionTool::handleSession(HttpRequest & request,
														HttpResponse & response,
														HttpSessionManager & sessionManager) {
		string sessionId = getSessionId(request);
		AutoRef<HttpSession> session;
		
		if (sessionId.empty() || !sessionManager.hasSession(sessionId)) {
			session = createSession(request, response, sessionManager);
		} else {
			session = sessionManager.getSession(sessionId);
		}
		
		if (session->outdated()) {
			sessionManager.destroySession(session->id());
			session = createSession(request, response, sessionManager);
		}
		
		if (isCookieSession(request) == false) {
			setCookieSession(request, response, session);
		}
		return session;
	}

	AutoRef<HttpSession> HttpSessionTool::createSession(HttpRequest & request,
														HttpResponse & response,
														HttpSessionManager & sessionManager) {
		AutoRef<HttpSession> session = sessionManager.createSession();
		setCookieSession(request, response, session);
		return session;
	}

	string HttpSessionTool::url(HttpRequest & request,
								const string & u,
								AutoRef<HttpSession> session) {
		if (request.getCookie(KEY_SESSION_ID) == session->id()) {
			return u;
		}
		size_t f = u.find("?");
		string path = (f == string::npos) ? u : u.substr(0, f);
		string rest = (f == string::npos) ? "" : u.substr(f);
		return path + ";" + KEY_SESSION_ID + "=" + session->id() + rest;
	}
}
