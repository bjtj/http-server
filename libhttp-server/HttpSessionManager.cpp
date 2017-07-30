#include "HttpSessionManager.hpp"
#include <liboslayer/AutoLock.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
    using namespace UTIL;
	
	HttpSessionManager::HttpSessionManager(unsigned long timeout)
		: id_idx(0), timeout(timeout), sem(1) {
	}
	HttpSessionManager::~HttpSessionManager() {
		clear();
	}

	std::string HttpSessionManager::genSessionId() {
		return Text::toString(id_idx++);
	}

	void HttpSessionManager::clear() {
		sem.wait();
		sessions.clear();
		sem.post();
	}

	void HttpSessionManager::removeOutdatedSessions() {
		sem.wait();
		for (vector< AutoRef<HttpSession> >::iterator iter = sessions.begin();
			 iter != sessions.end();) {
			if ((*iter)->outdated()) {
				iter = sessions.erase(iter);
			} else {
				iter++;
			}
		}
		sem.post();
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
	
	AutoRef<HttpSession> HttpSessionManager::getSession(const string & id) {
		OS::AutoLock lock((Ref<Semaphore>(&sem)));
		for (vector< AutoRef<HttpSession> >::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->id() == id) {
				return *iter;
			}
		}
		throw Exception("session not found", -1, 0);
	}
	
	AutoRef<HttpSession> HttpSessionManager::createSession() {
        OS::AutoLock lock((Ref<Semaphore>(&sem)));
		AutoRef<HttpSession> session(new HttpSession(genSessionId()));
		session->timeout() = timeout;
		sessions.push_back(session);
		return *sessions.rbegin();
	}
	
	void HttpSessionManager::destroySession(const string & id) {
		sem.wait();
		for (vector< AutoRef<HttpSession> >::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->id() == id) {
				sessions.erase(iter);
				break;
			}
		}
		sem.post();
	}

	vector< AutoRef<HttpSession> > & HttpSessionManager::getSessions() {
		return sessions;
	}
	
}
