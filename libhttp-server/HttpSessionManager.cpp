#include "HttpSessionManager.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	
	HttpSessionManager::HttpSessionManager(unsigned long timeout)
		: timeout(timeout) {
	}
	HttpSessionManager::~HttpSessionManager() {
		clear();
	}

	void HttpSessionManager::clear() {
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			delete *iter;
		}
		sessions.clear();
	}

	void HttpSessionManager::removeOutdatedSessions() {
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end();) {
			if ((*iter)->outdated()) {
				delete *iter;
				iter = sessions.erase(iter);
			} else {
				iter++;
			}
		}
	}
	
	bool HttpSessionManager::hasSession(unsigned long id) {
		try {
			getSession(id);
			return true;
		} catch (Exception & e) {
			return false;
		}
	}
	
	HttpSession & HttpSessionManager::getSession(unsigned long id) {
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->getId() == id) {
				return **iter;
			}
		}
		throw Exception("session not found", -1, 0);
	}
	
	HttpSession & HttpSessionManager::createSession() {
		HttpSession * session = new HttpSession;
		session->setTimeout(timeout);
		sessions.push_back(session);
		return **sessions.rbegin();
	}
	
	void HttpSessionManager::destroySession(unsigned long id) {
		for (vector<HttpSession*>::iterator iter = sessions.begin();
			 iter != sessions.end(); iter++) {
			if ((*iter)->getId() == id) {
				delete *iter;
				sessions.erase(iter);
				break;
			}
		}
	}

	vector<HttpSession*> & HttpSessionManager::getSessions() {
		return sessions;
	}
	
}
