#include <cstring>
#include <sys/stat.h>
#include <dirent.h>

#include "HttpRequestHandlers.hpp"
#include "os.hpp"
#include "Text.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	FileRedirectHandler::FileRedirectHandler(string basePath) : basePath(basePath) {
	}
	
	FileRedirectHandler::~FileRedirectHandler() {
	}
	
	void FileRedirectHandler::onRequest(HttpRequest & request, HttpResponse & response) {

		if (!request.remaining()) {

			bool found = false;
			string relativePath = request.getPath();
			string fullpath = getFullPath(relativePath);

			if (OS::File::isFile(fullpath)) {
				found = onFile(relativePath, response);
			} else if (OS::File::isDirectory(fullpath)) {
				found = onDirectory(relativePath, response);
			} else {
				found = false;
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
	
	bool FileRedirectHandler::onFile(string relativePath, HttpResponse & response) {

		string fullpath = getFullPath(relativePath);
		FILE * fp = fopen(fullpath.c_str(), "rb");
		if (!fp) {
			return false;
		}

		response.setContentLength(getFileSize(fullpath));
		response.setContentType(getContentType(fullpath));
		
		char buf[1024] = {0,};
		int len = 0;
		while ((len = fread(buf, 1, sizeof(buf), fp)) > 0) {
			response.send(buf, len);
		}
		fclose(fp);
		return true;
	}

	bool FileRedirectHandler::onDirectory(string relativePath, HttpResponse & response) {

		string fullpath = getFullPath(relativePath);
		int cnt, i;
		struct dirent * ent = NULL;
		struct dirent ** list = NULL;
		cnt = scandir(fullpath.c_str(), &list, NULL, alphasort);
		
		response.setContentType("text/html");
		
		response.write("<html>");
		response.write("<body>");
		response.write("<ul>");
		for (i = 0; i < cnt; i++) {

			ent = list[i];

			string entPath = OS::File::fullpath(fullpath, ent->d_name);
			bool isDir = OS::File::isDirectory(entPath);
			char buf[1024] = {0,};
			snprintf(buf, sizeof(buf), "<li><a href=\"%s\">%s%s</a></li>",
					 OS::File::fullpath(relativePath, ent->d_name).substr(1).c_str(),
					 ent->d_name,
					 (isDir ? "/" : ""));
			response.write(buf, strlen(buf));
		}
		response.write("</ul>");
		response.write("</body>");
		response.write("</html>");

		return true;
	}

	string FileRedirectHandler::getFullPath(const string & path) {
		return OS::File::fullpath(basePath, path);
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

