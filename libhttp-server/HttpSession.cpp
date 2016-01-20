#include "HttpSession.hpp"

namespace HTTP {

	unsigned long HttpSession::id_idx = 0;

	HttpSession::HttpSession(unsigned long creationTime) : lastAccessTime(creationTime) {
		id = id_idx++;
	}
	HttpSession::~HttpSession() {
	}

	unsigned long HttpSession::getLastAccessTime() {
		return lastAccessTime;
	}
	void HttpSession::setLastAccessTime(unsigned long time) {
		this->lastAccessTime = time;
	}
	bool HttpSession::testOutdated(unsigned long currentTime, unsigned long timeout) {
		return (currentTime - lastAccessTime > timeout);
	}

	std::string & HttpSession::operator[](const std::string & name) {
		return props[name];
	}
}
