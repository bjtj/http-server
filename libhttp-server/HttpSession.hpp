#ifndef __HTTP_SESSION_HPP__
#define __HTTP_SESSION_HPP__

#include <string>
#include <liboslayer/os.hpp>
#include <liboslayer/StringElements.hpp>

namespace http {

	/**
	 * http session
	 */
	class HttpSession {
	private:
		std::string _id;
		unsigned long _creationTime;
		unsigned long _lastAccessTime;
		unsigned long _timeout;
		osl::StringMap _props;
	public:
		HttpSession(const std::string & id);
		virtual ~HttpSession();
		std::string & id();
		std::string id() const;
		void updateLastAccessTime();
		unsigned long & timeout();
		unsigned long timeout() const;
		bool outdated() const;
		unsigned long remainingLife() const;
		std::string & operator[](const std::string & name);
	};
}

#endif
