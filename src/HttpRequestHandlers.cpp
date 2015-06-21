#include <cstring>
#include <sys/stat.h>
#include <dirent.h>

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

			bool found = false;
			string uri = request.getPath();
			string fullpath = path + uri;

			if (OS::File::exists(fullpath)) {

				if (OS::File::isFile(fullpath)) {
					
					found = onFile(fullpath, response);
					
				} else {
					
					found = onDirectory(fullpath, response);
					
				}
			}

			if (!found) {
				response.setStatusCode(404);
				response.write("404 Not Found");
			}
			
			response.setComplete();
		}
	}

	int FileRedirectHandler::getFileSize(string path) {
		struct stat st;
		if (stat(path.c_str(), &st) == 0) {
			return st.st_size;
		}
		return 0;
	}
	
	string FileRedirectHandler::getContentType(string path) {
		if (Text::endsWith(path, ".html")) {
			return "text/html";
		}
		return "text/plain";
	}
	
	bool FileRedirectHandler::onFile(string path, HttpResponse & response) {
		
		FILE * fp = fopen(path.c_str(), "rb");
		if (!fp) {
			return false;
		}

		response.setContentLength(getFileSize(path));
		response.setContentType(getContentType(path));
		
		char buf[1024] = {0,};
		int len = 0;
		while ((len = fread(buf, 1, sizeof(buf), fp)) > 0) {
			response.send(buf, len);
		}
		fclose(fp);
		return true;
	}

	bool FileRedirectHandler::onDirectory(string path, HttpResponse & response) {

		int cnt, i;
		struct dirent * ent = NULL;
		struct dirent ** list = NULL;
		cnt = scandir(path.c_str(), &list, NULL, alphasort);
		
		response.setContentType("text/html");
		
		response.write("<html>");
		response.write("<body>");
		response.write("<ul>");
		for (i = 0; i < cnt; i++) {

			ent = list[i];

			bool isDir = OS::File::isDirectory(path + "/" + ent->d_name);
			
			char buf[1024] = {0,};
			snprintf(buf, sizeof(buf), "<li><a href=\"%s/%s\">%s%s</a></li>",
					 path.c_str(),
					 ent->d_name,
					 ent->d_name,
					 (isDir ? "/" : ""));
			response.write(buf, strlen(buf));
		}
		response.write("</ul>");
		response.write("</body>");
		response.write("</html>");

		return true;
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

