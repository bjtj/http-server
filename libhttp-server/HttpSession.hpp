#ifndef __HTTP_SESSION_HPP__
#define __HTTP_SESSION_HPP__

#include <liboslayer/StringElement.hpp>

namespace HTTP {

	class HttpSession {
	private:
		static unsigned long id_idx;
		unsigned long id;
		unsigned long lastAccessTime;
		UTIL::StringMap props;
	public:
		HttpSession(unsigned long creationTime);
		virtual ~HttpSession();

		unsigned long getLastAccessTime();
		void setLastAccessTime(unsigned long time);
		bool testOutdated(unsigned long currentTime, unsigned long timeout);

		std::string & operator[](const std::string & name);
	};
}

#endif
