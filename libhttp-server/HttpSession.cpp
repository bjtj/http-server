#include "HttpSession.hpp"

using namespace OS;

namespace HTTP {

	static unsigned long s_tick() {
		return tick_milli();
	}

	HttpSession::HttpSession(unsigned long id) : id(id) {
		creationTime = s_tick();
		lastAccessTime = s_tick();
	}
	
	HttpSession::~HttpSession() {
	}

	unsigned long HttpSession::getId() const {
		return id;
	}

	void HttpSession::updateLastAccessTime() {
		lastAccessTime = s_tick();
	}
	
	unsigned long HttpSession::getTimeout() {
		return timeout;
	}
	
	void HttpSession::setTimeout(unsigned long timeout) {
		this->timeout = timeout;
	}
	
	bool HttpSession::outdated() {
		return (s_tick() - lastAccessTime > timeout);
	}

	unsigned long HttpSession::remainingLife() {
		unsigned long ellapsed = s_tick() - lastAccessTime;
		return (ellapsed < timeout) ? timeout - ellapsed : 0;
	}

	std::string & HttpSession::operator[](const std::string & name) {
		return props[name];
	}
}
