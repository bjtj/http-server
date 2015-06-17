#include <iostream>
#include "HttpRequestHandlers.hpp"
#include "os.hpp"

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
	
}

