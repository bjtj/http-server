#ifndef __HTTP_SESSION_HPP__
#define __HTTP_SESSION_HPP__

#include <liboslayer/StringElement.hpp>

namespace HTTP {

	class HttpSession {
	private:
		static unsigned long id_idx;
		unsigned long id;
		unsigned long creationTime;
		unsigned long lastAccessTime;
		unsigned long timeout;
		UTIL::StringMap props;
		
	public:
		HttpSession();
		virtual ~HttpSession();

		unsigned long getId();
		void updateLastAccessTime();
		unsigned long getTimeout();
		void setTimeout(unsigned long timeout);
		bool oudated();
		
		std::string & operator[](const std::string & name);
	};
}

#endif
