#ifndef __HTTP_SESSION_MANAGER_HPP__
#define __HTTP_SESSION_MANAGER_HPP__

#include <liboslayer/os.hpp>
#include <vector>
#include "HttpSession.hpp"

namespace HTTP {
	class HttpSessionManager {
	private:
		unsigned long timeout;
		std::vector<HttpSession*> sessions;
		OS::Semaphore sem;

	private:
		/* do not allow copy */
		HttpSessionManager(const HttpSessionManager & other);
		HttpSessionManager & operator=(const HttpSessionManager & other);
	
	public:
		HttpSessionManager(unsigned long timeout);
		virtual ~HttpSessionManager();
		void clear();
		void removeOutdatedSessions();
		bool hasSession(unsigned long id);
		HttpSession & getSession(unsigned long id);
		HttpSession & createSession();
		void destroySession(unsigned long id);
		std::vector<HttpSession*> & getSessions();
	};
}

#endif
