#ifndef __COOKIE_HPP__
#define __COOKIE_HPP__

#include <liboslayer/StringElements.hpp>
#include <string>

namespace HTTP {

	/**
	 * cookie
	 */
	class Cookie {
	private:
		UTIL::LinkedStringMap _components;
	public:
		Cookie();
		Cookie(const std::string & phrase);
		Cookie(const UTIL::LinkedStringMap & components);
		virtual ~Cookie();
		UTIL::LinkedStringMap & components();
		bool contains(const std::string & key);
		bool containsIgnoreCase(const std::string & key);
		std::string toString() const;
		static UTIL::LinkedStringMap parse(const std::string & phrase);
		static std::string toString(const UTIL::LinkedStringMap & components);
		std::string & operator[] (const std::string & name);
	};
}

#endif
