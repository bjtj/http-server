#include "BasicAuth.hpp"
#include <liboslayer/Base64.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {
	
	using namespace std;
	using namespace UTIL;

	OnBasicAuth::OnBasicAuth() {
	}
	OnBasicAuth::~OnBasicAuth() {
	}

	class SimpleOnBasicAuth : public OnBasicAuth {
	private:
		string _username;
		string _password;
	public:
		SimpleOnBasicAuth(const string & username, const string & password)
			: _username(username), _password(password) {
		}
		virtual ~SimpleOnBasicAuth() {}

		virtual bool onAuth(const string & username, const string & password) {
			return (_username == username && _password == password);
		}
	};

	
	BasicAuth::BasicAuth(AutoRef<OnBasicAuth> onAuth) : onAuth(onAuth) {
	}
	BasicAuth::BasicAuth(const string & username, const string & password)
		: onAuth(new SimpleOnBasicAuth(username, password)) {
	}
	BasicAuth::BasicAuth(const string & realm, const string & username, const string & password)
		: HttpAuth(realm), onAuth(new SimpleOnBasicAuth(username, password)) {
	}
	BasicAuth::~BasicAuth() {
	}
	bool BasicAuth::validate(HttpRequest & request) {

		string auth = request.getHeaderField("Authorization");
		if (auth.empty()) {
			return false;
		}

		string username;
		string password;

		string prefix = "Basic ";
		if (!Text::startsWith(auth, prefix)) {
			return false;
		}
		
		string encoded = auth.substr(prefix.size());
		string decoded = Base64::decode(encoded);
		size_t f = decoded.find(":");
		if (f == string::npos) {
			username = decoded;
		} else {
			username = decoded.substr(0, f);
			password = decoded.substr(f + 1);
		}

		if (!onAuth.nil() && onAuth->onAuth(username, password)) {
			return true;
		}

		return false;
	}
	void BasicAuth::setAuthentication(HttpResponse & response) {
		response.setStatus(401);
		response.getHeader().setHeaderField("WWW-Authenticate", "Basic realm=\"" + realm() + "\"");
	}
}
