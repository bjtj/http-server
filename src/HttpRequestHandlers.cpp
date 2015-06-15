#include "HttpRequestHandlers.hpp"
#include <iostream>

namespace HTTP {

	using namespace std;
	
	FileRedirectHandler::FileRedirectHandler(string path) : path(path) {
	}
	
	FileRedirectHandler::~FileRedirectHandler() {
	}
	
	void FileRedirectHandler::onRequest(HttpRequest & request, HttpResponse & response) {

		if (!request.remaining()) {

			string uri = request.getPath();
			string fullpath = path + uri;

			FILE * fp = fopen(fullpath.c_str(), "rb");
			if (!fp) {
				response.setStatusCode(404);
			} else {
				char buf[1024] = {0,};
				int len = 0;
				while ((len = fread(buf, 1, sizeof(buf), fp)) > 0) {
					response.write(buf, len);
				}
				fclose(fp);

				response.setContentType("text/html");
			}
			
			response.setComplete();
		}
	}
	
}

