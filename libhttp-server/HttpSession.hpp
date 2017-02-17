#ifndef __HTTP_SESSION_HPP__
#define __HTTP_SESSION_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/StringElements.hpp>

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
