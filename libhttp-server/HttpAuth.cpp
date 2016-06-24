#include "HttpAuth.hpp"

namespace HTTP {
	
	HttpAuth::HttpAuth() {}
	HttpAuth::~HttpAuth() {}
	std::string & HttpAuth::realm() {
		return _realm;
	}
	
}
