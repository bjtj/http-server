#ifndef __LOGGER_HPP__
#define __LOGGER_HPP__

#include <string>

namespace HTTP {
	
	class Logger {
	private:		
	public:
		Logger();
		virtual ~Logger();

		static Logger & getLogger();

		virtual void logd(const char * msg) = 0;
		virtual void logd(const std::string & msg) = 0;
		virtual void loge(const char * msg) = 0;
		virtual void loge(const std::string & msg) = 0;
	};
}

#endif
