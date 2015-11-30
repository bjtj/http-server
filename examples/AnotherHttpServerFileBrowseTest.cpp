#include <liboslayer/os.hpp>
#include <liboslayer/Utils.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

class FileBrowseHttpRequestHandler : public HttpRequestHandler {
private:

public:
	FileBrowseHttpRequestHandler() {}
	virtual ~FileBrowseHttpRequestHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/html");
		responseHeader.setConnection("close");

		string path = request.getParameter("path");
		if (path.empty()) {
			path = ".";
		}

		string content;

		try {

			vector<File> files = File::list(path);

			content.append("<html>");
			content.append("<head>");
			content.append("</head>");
			content.append("<body>");
			content.append("<ul>");
			for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
				content.append("<li>");
				if (iter->isDirectory()) {
					content.append("<a href=\"browse?path=" + File::mergePaths(path, iter->getName()) + "\">");
					content.append(iter->getName());
					content.append("</a>");
				} else {
					content.append(iter->getName());
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

size_t readline(char * buffer, size_t max) {
	fgets(buffer, (int)max - 1, stdin);
	buffer[strlen(buffer) - 1] = 0;
	return strlen(buffer);
}

int main(int argc, char * args[]) {

	AnotherHttpServer server(8083);

	FileBrowseHttpRequestHandler browse;
	server.registerRequestHandler("/browse", &browse);

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