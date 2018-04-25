#ifndef __HTTP_SERVER_CONFIG_HPP__
#define __HTTP_SERVER_CONFIG_HPP__

#include <liboslayer/Properties.hpp>
#include <string>

namespace http {

	class HttpServerConfig : public osl::Properties {
	private:
	public:
		HttpServerConfig() {}
		HttpServerConfig(int port) { setProperty("listen.port", port); }
		virtual ~HttpServerConfig() {}
	};

}

#endif
