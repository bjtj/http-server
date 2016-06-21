#include "BasicAuth.hpp"
#include <liboslayer/Base64.hpp>

namespace HTTP {
	
	using namespace std;
	using namespace UTIL;
	
	BasicAuth::BasicAuth(const string & username, const string & password)
		: _username(username), _password(password) {
	}
	BasicAuth::~BasicAuth() {
	}
	bool BasicAuth::validate(HttpRequest & request) {
		string base64 = Base64::encode(_username + ":" + _password);
		return request.getHeaderField("Authorization") == ("Basic " + base64);
	}
	void BasicAuth::setAuthentication(HttpResponse & response) {
		response.setStatusCode(401);
		response.getHeader().setHeaderField("WWW-Authenticate", "Basic realm=\"Basic Auth Test\"");
	}
	string & BasicAuth::username() {
		return _username;
	}
	string & BasicAuth::password() {
		return _password;
	}
	bool BasicAuth::empty() {
		return _username.empty() && _password.empty();
	}
}
