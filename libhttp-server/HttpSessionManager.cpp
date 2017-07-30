#include "HttpSessionManager.hpp"
#include <liboslayer/AutoLock.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
    using namespace UTIL;
	
	HttpSessionManager::HttpSessionManager(unsigned long timeout)
		: _id_idx(0), _timeout(timeout), _sem(1) {
	}
	HttpSessionManager::~HttpSessionManager() {
		clear();
	}

	std::string HttpSessionManager::genSessionId() {
		return Text::toString(_id_idx++);
	}

	void HttpSessionManager::clear() {
		_sem.wait();
		_sessions.clear();
		_sem.post();
	}

	void HttpSessionManager::removeOutdatedSessions() {
		_sem.wait();
		for (vector< AutoRef<HttpSession> >::iterator iter = _sessions.begin();
			 iter != _sessions.end();) {
			if ((*iter)->outdated()) {
				iter = _sessions.erase(iter);
			} else {
				iter++;
			}
		}
		_sem.post();
	}
	
	bool HttpSessionManager::hasSession(const string & id) {
		try {
			getSession(id);
			return true;
		} catch (Exception & e) {
			(void)e;
			return false;
		}
	}

	unsigned long & HttpSessionManager::timeout() {
		return _timeout;
	}
	
	AutoRef<HttpSession> HttpSessionManager::getSession(const string & id) {
		OS::AutoLock lock((Ref<Semaphore>(&_sem)));
		for (vector< AutoRef<HttpSession> >::iterator iter = _sessions.begin();
			 iter != _sessions.end(); iter++) {
			if ((*iter)->id() == id) {
				return *iter;
			}
		}
		throw Exception("session not found", -1, 0);
	}
	
	AutoRef<HttpSession> HttpSessionManager::createSession() {
        OS::AutoLock lock((Ref<Semaphore>(&_sem)));
		AutoRef<HttpSession> session(new HttpSession(genSessionId()));
		session->timeout() = _timeout;
		_sessions.push_back(session);
		return *_sessions.rbegin();
	}
	
	void HttpSessionManager::destroySession(const string & id) {
		_sem.wait();
		for (vector< AutoRef<HttpSession> >::iterator iter = _sessions.begin();
			 iter != _sessions.end(); iter++) {
			if ((*iter)->id() == id) {
				_sessions.erase(iter);
				break;
			}
		}
		_sem.post();
	}

	vector< AutoRef<HttpSession> > & HttpSessionManager::getSessions() {
		return _sessions;
	}
	
}
