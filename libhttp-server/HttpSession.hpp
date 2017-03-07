#ifndef __HTTP_SESSION_HPP__
#define __HTTP_SESSION_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/StringElements.hpp>

namespace HTTP {

	/**
	 * http session
	 */
	class HttpSession {
	private:
		unsigned long id;
		unsigned long creationTime;
		unsigned long lastAccessTime;
		unsigned long timeout;
		UTIL::StringMap props;
	public:
		HttpSession(unsigned long id);
		virtual ~HttpSession();
		unsigned long getId() const;
		void updateLastAccessTime();
		unsigned long getTimeout();
		void setTimeout(unsigned long timeout);
		bool outdated();
		unsigned long remainingLife();
		std::string & operator[](const std::string & name);
	};
}

#endif
