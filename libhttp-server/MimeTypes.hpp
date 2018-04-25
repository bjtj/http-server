#ifndef __MIME_TYPES_HPP__
#define __MIME_TYPES_HPP__

#include <string>
#include <map>

namespace http {

	class MimeTypes {
	private:
		static std::map<std::string, std::string> types;
	public:
		MimeTypes();
		virtual ~MimeTypes();
		static void clear();
		static void load(const std::string & path);
		static std::map<std::string, std::string> getMimeTypes();
		static std::string getMimeType(const std::string & ext);
	};

}


#endif
