#ifndef __COOKIE_HPP__
#define __COOKIE_HPP__

#include <liboslayer/StringElements.hpp>
#include <string>

namespace http {

	/**
	 * cookie
	 */
	class Cookie {
	private:
		osl::LinkedStringMap _components;
	public:
		Cookie();
		Cookie(const std::string & phrase);
		Cookie(const osl::LinkedStringMap & components);
		virtual ~Cookie();
		osl::LinkedStringMap & components();
		bool contains(const std::string & key);
		bool containsIgnoreCase(const std::string & key);
		std::string toString() const;
		static osl::LinkedStringMap parse(const std::string & phrase);
		static std::string toString(const osl::LinkedStringMap & components);
		std::string & operator[] (const std::string & name);
	};
}

#endif
