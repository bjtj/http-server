#ifndef __HTTP_SESSION_MANAGER_HPP__
#define __HTTP_SESSION_MANAGER_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Semaphore.hpp>
#include <vector>
#include "HttpSession.hpp"

namespace HTTP {

	/**
	 * http session manager
	 */
	class HttpSessionManager {
	private:
		unsigned long id_idx;
		unsigned long timeout;
        std::vector< OS::AutoRef<HttpSession> > sessions;
		OS::Semaphore sem;
	private:
		/* do not allow copy */
		HttpSessionManager(const HttpSessionManager & other);
		HttpSessionManager & operator=(const HttpSessionManager & other);
	public:
		HttpSessionManager(unsigned long timeout);
		virtual ~HttpSessionManager();
		std::string genSessionId();
		void clear();
		void removeOutdatedSessions();
		bool hasSession(const std::string & id);
		OS::AutoRef<HttpSession> getSession(const std::string & id);
		OS::AutoRef<HttpSession> createSession();
		void destroySession(const std::string & id);
		std::vector< OS::AutoRef<HttpSession> > & getSessions();
	};
}

#endif
