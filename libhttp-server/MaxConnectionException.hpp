#ifndef __MAX_CONNECTION_EXCEPTION_HPP__
#define __MAX_CONNECTION_EXCEPTION_HPP__

#include <liboslayer/os.hpp>
#include <string>

namespace HTTP {

	class MaxConnectionException : public OS::Exception
	{
	public:
		MaxConnectionException() {}
		MaxConnectionException(const std::string & msg) : Exception(msg) {}
		MaxConnectionException(const char * msg) : Exception(msg) {}
		virtual ~MaxConnectionException() throw() {}
	};

}

#endif
