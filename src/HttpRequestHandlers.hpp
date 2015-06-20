#ifndef __HTTP_REQUEST_HANDLERS_HPP__
#define __HTTP_REQUEST_HANDLERS_HPP__

#include <string>
#include "HttpProtocol.hpp"

namespace HTTP {
	
	/**
	 * @brief file redirect handler
	 */
	class FileRedirectHandler : public HttpRequestHandler {
	private:
		std::string path;
	public:
		FileRedirectHandler(std::string path);
		virtual ~FileRedirectHandler();

		virtual void onRequest(HttpRequest & request, HttpResponse & response);
	};

	/**
	 * @brief REST handler
	 */
	class RESThandler : public HttpRequestHandler {
	public:
		RESThandler();
		virtual ~RESThandler();

		virtual void onRequest(HttpRequest & request, HttpResponse & response);
		
		virtual void onPost(HttpRequest & request, HttpResponse & response);
		virtual void onGet(HttpRequest & request, HttpResponse & response);
		virtual void onPut(HttpRequest & request, HttpResponse & response);
		virtual void onDelete(HttpRequest & request, HttpResponse & response);
		
	};

}

#endif
