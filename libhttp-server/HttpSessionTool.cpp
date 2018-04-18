#include "HttpSessionTool.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;

	const string HttpSessionTool::KEY_SESSION_ID = "sessionId";

	static const unsigned long long HOUR = 60ULL * 60ULL;
	static const unsigned long long DAY = 24ULL * HOUR;
	static const unsigned long long MONTH = 30ULL * DAY;
	static const unsigned long long YEAR = 365ULL * DAY;
	
	HttpSessionTool::HttpSessionTool() : _path("/") {
		_expire.sec = MONTH;
		_expire.nano = 0;
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
		string date = Date::formatRfc1123(Date::now() + Date(_expire));
		Cookie cookie(KEY_SESSION_ID + "=" + session->id() +
					  "; expires=" + date +
					  "; path=" + _path +
					  "; HttpOnly");
		response.removeHeaderFieldsCase("Set-Cookie");
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

	osl_time_t & HttpSessionTool::expire() {
		return _expire;
	}

	string & HttpSessionTool::path() {
		return _path;
	}
}
