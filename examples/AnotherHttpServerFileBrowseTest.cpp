#include <liboslayer/os.hpp>
#include <liboslayer/Utils.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/FileTransfer.hpp>
#include <libhttp-server/HttpEncoderDecoder.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

class FileBrowseHttpRequestHandler : public HttpRequestHandler {
private:
	string defaultPath;
public:
	FileBrowseHttpRequestHandler(const string & defaultPath) : defaultPath(defaultPath) {}
	virtual ~FileBrowseHttpRequestHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/html");
		responseHeader.setConnection("close");

		string path = request.getParameter("path");
		if (path.empty()) {
			path = defaultPath;
		}

		path = HttpDecoder::decode(path);

		string content;

		try {

			vector<File> files = File::list(path);

			content.append("<html>");
			content.append("<head>");
			content.append("</head>");
			content.append("<body>");
			// content.append("<form>Path: <input type=\"text\" name=\"path\"/></form>");
			content.append("<ul>");
			for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {

				if (iter->isDirectory() && (iter->getName().compare("..") || iter->getName().compare("."))) {
					continue;
				}
				
				content.append("<li>");
				if (iter->isDirectory()) {
					content.append("<span style=\"display:inline-block;width:15px;\">D</span>");
					content.append("<a href=\"browse?path=" + File::mergePaths(path, iter->getName()) + "\">");
					content.append(iter->getName());					
					content.append("</a>");
				} else {
					content.append("<span style=\"display:inline-block;width:15px;\">&nbsp;</span>");
					content.append("<a href=\"file?path=" + File::mergePaths(path, iter->getName()) + "\">");
					content.append(iter->getName());
					content.append("</a>");
				}
				content.append("</li>");
			}
			content.append("</ul>");
			content.append("</body>");
			content.append("</html>");

		} catch (IOException e) {
			content = "Access failed : " + path;
		}

        setFixedTransfer(response, content);
    }
    
    virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
    }
};

class FileDownloadHttpRequestHandler : public HttpRequestHandler {
private:
public:
	FileDownloadHttpRequestHandler() {}
	virtual ~FileDownloadHttpRequestHandler() {}

	virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/plain");
		responseHeader.setConnection("close");

		string path = request.getParameter("path");
		if (path.empty()) {
			response.setStatusCode(404);
			setFixedTransfer(response, "File not found / path: " + path);
			return;
		}

		File file(path);
		if (!file.isFile()) {
			response.setStatusCode(400);
			setFixedTransfer(response, "Not a file / path: " + path);
			return;
		}

		string type = guessContentType(path);
		if (!type.empty()) {
			response.setContentType(type);
		} else {
			response.setContentType("Application/octet-stream");
			response.getHeader().setHeaderField("Content-Disposition", "attachment; filename=\"" + file.getName() + "\"");
		}

		setFileTransfer(response, file);
    }

	string guessContentType(const string & path) {
		string ext = File::getExtension(path);
		if (ext.empty()) {
			return "";
		}

		map<string, string> types;
		types["txt"] = "text/plain";
		types["htm"] = "text/html";
		types["html"] = "text/html";
		types["css"] = "text/css";
		types["js"] = "text/javascript";
		types["xml"] = "text/xml";

		for (map<string, string>::iterator iter = types.begin(); iter != types.end(); iter++) {
			if (Text::equalsIgnoreCase(ext, iter->first)) {
				return iter->second;
			}
		}

		return "";
	}
    
    virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
    }
};

size_t readline(char * buffer, size_t max) {
	if (fgets(buffer, (int)max - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return strlen(buffer);
	}
	return 0;
}

int main(int argc, char * args[]) {

	string path = ".";
	if (argc > 1) {
		path = args[1];
	}

	AnotherHttpServer server(8083);

	FileBrowseHttpRequestHandler browse(path);
	server.registerRequestHandler("/browse", &browse);
	FileDownloadHttpRequestHandler file;
	server.registerRequestHandler("/file", &file);

	server.startAsync();

	while (1) {
		char buffer[1024] = {0,};
		if (readline(buffer, sizeof(buffer)) > 0) {
			if (!strcmp(buffer, "q")) {
				break;
			}
		}
	}

	server.stop();
}
