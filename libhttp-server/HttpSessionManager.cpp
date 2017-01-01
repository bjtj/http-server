#include "HttpSessionManager.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	
	HttpSessionManager::HttpSessionManager(unsigned long timeout)
		: timeout(timeout), sem(1) {
	}
	HttpSessionManager::~HttpSessionManager() {
		clear();
	}

	void HttpSessionManager::clear() {
		sem.wait();
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			delete *iter;
		}
		sessions.clear();
		sem.post();
	}

	void HttpSessionManager::removeOutdatedSessions() {
		sem.wait();
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end();) {
			if ((*iter)->outdated()) {
				delete *iter;
				iter = sessions.erase(iter);
			} else {
				iter++;
			}
		}
		sem.post();
	}
	
	bool HttpSessionManager::hasSession(unsigned long id) {
		try {
			getSession(id);
			return true;
		} catch (Exception & e) {
			(void)e;
			return false;
		}
	}
	
	HttpSession & HttpSessionManager::getSession(unsigned long id) {
		OS::AutoLock lock(sem);
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->getId() == id) {
				return **iter;
			}
		}
		throw Exception("session not found", -1, 0);
	}
	
	HttpSession & HttpSessionManager::createSession() {
		sem.wait();
		HttpSession * session = new HttpSession;
		session->setTimeout(timeout);
		sessions.push_back(session);
		return **sessions.rbegin();
		sem.post();
	}
	
	void HttpSessionManager::destroySession(unsigned long id) {
		sem.wait();
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->getId() == id) {
				delete *iter;
				sessions.erase(iter);
				break;
			}
		}
		sem.post();
	}

	vector<HttpSession*> & HttpSessionManager::getSessions() {
		return sessions;
	}
	
}
