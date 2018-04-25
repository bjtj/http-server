#include "HttpAuth.hpp"

namespace http {

	using namespace std;
	
	HttpAuth::HttpAuth() {}
	HttpAuth::HttpAuth(const string & realm) : _realm(realm) {}
	HttpAuth::~HttpAuth() {}
	std::string & HttpAuth::realm() {
		return _realm;
	}
	
}
