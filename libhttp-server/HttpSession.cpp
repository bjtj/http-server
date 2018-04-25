#include "HttpSession.hpp"


namespace http {

	using namespace std;
	using namespace osl;

	static unsigned long s_tick() {
		return tick_milli();
	}

	HttpSession::HttpSession(const string & id) : _id(id) {
		_creationTime = s_tick();
		_lastAccessTime = s_tick();
	}
	
	HttpSession::~HttpSession() {
	}

	string & HttpSession::id() {
		return _id;
	}

	string HttpSession::id() const {
		return _id;
	}

	void HttpSession::updateLastAccessTime() {
		_lastAccessTime = s_tick();
	}
	
	unsigned long & HttpSession::timeout() {
		return _timeout;
	}

	unsigned long HttpSession::timeout() const {
		return _timeout;
	}
	
	bool HttpSession::outdated() const {
		return (s_tick() - _lastAccessTime > _timeout);
	}

	unsigned long HttpSession::remainingLife() const {
		unsigned long ellapsed = s_tick() - _lastAccessTime;
		return (ellapsed < _timeout) ? _timeout - ellapsed : 0;
	}

	std::string & HttpSession::operator[](const std::string & name) {
		return _props[name];
	}
}
