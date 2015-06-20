#include <iostream>
#include "HttpRequestHandlers.hpp"
#include "os.hpp"
#include "Text.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	FileRedirectHandler::FileRedirectHandler(string path) : path(path) {
	}
	
	FileRedirectHandler::~FileRedirectHandler() {
	}
	
	void FileRedirectHandler::onRequest(HttpRequest & request, HttpResponse & response) {

		if (!request.remaining()) {

			string uri = request.getPath();
			string fullpath = path + uri;

			if (!OS::File::isFile(fullpath)) {
				response.setStatusCode(404);
				response.write("404 not found");
			} else {
				FILE * fp = fopen(fullpath.c_str(), "rb");
				if (!fp) {
					response.setStatusCode(404);
					response.write("404 not found");
				} else {
					char buf[1024] = {0,};
					int len = 0;
					while ((len = fread(buf, 1, sizeof(buf), fp)) > 0) {
						response.write(buf, len);
					}
					fclose(fp);
					
					response.setContentType("text/html");
				}
			}
			
			response.setComplete();
		}
	}




	RESThandler::RESThandler() {
	}
	RESThandler::~RESThandler() {
	}
	
	void RESThandler::onRequest(HttpRequest & request, HttpResponse & response) {
		
		string method = request.getMethod();
		
		if (Text::equalsIgnoreCase(method, "POST")) {
			onPost(request, response);
		} else if (Text::equalsIgnoreCase(method, "GET")) {
			onGet(request, response);
		} else if (Text::equalsIgnoreCase(method, "PUT")) {
			onPut(request, response);
		} else if (Text::equalsIgnoreCase(method, "DELETE")) {
			onDelete(request, response);
		} else {
		}
	}

	void RESThandler::onPost(HttpRequest & request, HttpResponse & response) {
	}
	void RESThandler::onGet(HttpRequest & request, HttpResponse & response) {
	}
	void RESThandler::onPut(HttpRequest & request, HttpResponse & response) {
	}
	void RESThandler::onDelete(HttpRequest & request, HttpResponse & response) {
	}
}

