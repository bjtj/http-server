#include <liboslayer/os.hpp>
#include <liboslayer/SecureSocket.hpp>
#include <liboslayer/Utils.hpp>
#include <liboslayer/Properties.hpp>
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
        
        printf("** Path: %s [%s:%d]\n", path.c_str(), request.getRemoteAddress().getHost().c_str(), request.getRemoteAddress().getPort());
        
        if (!Text::startsWith(path, defaultPath) || path.find("..") != string::npos) {
            response.setStatusCode(403);
            setFixedTransfer(response, "Not permitted");
            return;
        }

		bool debug = !request.getParameter("debug").empty();
        //bool login = !request.getParameter("pass").compare("secret");
        bool login = true;

		path = HttpDecoder::decode(HttpDecoder::decode_plus(path));

		string content;
        
        if (!login) {
            content.append("<!DOCTYPE html>");
            content.append("<html>");
            content.append("<head>");
            content.append("<title>browse</title>");
            content.append("<style type=\"text/css\">body {font-family: arial; font-size: 10pt;}</style>");
            content.append("<meta charset=\"UTF-8\">");
            content.append("</head>");
            content.append("<body>");
            content.append("<form method=\"GET\">Password: <input type=\"password\" name=\"pass\"/></form>");
            content.append("</body>");
            content.append("</html>");
        } else {
            content = renderDir(path, debug);
        }

        setFixedTransfer(response, content);
    }
    
    string renderDir(const string & path, bool debug) {
        
        string content;
        
        try {
            
            vector<File> files = File::list(path);
            
            content.append("<!DOCTYPE html>");
            content.append("<html>");
            content.append("<head>");
            content.append("<title>browse</title>");
            content.append("<style type=\"text/css\">body {font-family: arial; font-size: 10pt;}</style>");
            content.append("<meta charset=\"UTF-8\">");
            content.append("</head>");
            content.append("<body>");
            if (debug) {
                content.append("<form>Path: <input type=\"text\" name=\"path\"/></form>");
            }
            content.append("<h1>TJ Entertainment</h1>");
            content.append("<p><a href=\"browse\">home</a></p>");
            content.append("<ul>");
            for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
                
                if (!filter(*iter)) {
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
        
        return content;
    }

	bool filter(File & file) {
        if (Text::startsWith(file.getName(), ".")) {
			return false;
		}
		return true;
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

		path = HttpDecoder::decode(HttpDecoder::decode_plus(path));
		printf("** Path: %s [%s:%d]\n", path.c_str(), request.getRemoteAddress().getHost().c_str(), request.getRemoteAddress().getPort());

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

string prompt(const char * msg) {
    char buffer[1024] = {0,};
    printf("%s", msg);
    if (readline(buffer, sizeof(buffer)) > 0) {
        return string(buffer);
    }
    return "";
}
int promptInteger(const char * msg) {
    string ret = prompt(msg);
    return Text::toInt(ret);
}
bool promptBoolean(const char * msg) {
    string ret = prompt(msg);
    if (ret.compare("yes") || ret.compare("y")) {
        return true;
    }
    return false;
}

class ServerConfig {
private:
	int port;
	bool secure;
	string certPath;
    string keyPath;
	string defaultBrowsePath;
	
public:
    ServerConfig() : port(8083), secure(false), defaultBrowsePath(".") {}
    virtual ~ServerConfig() {}
	void load(const string & configPath) {
		Properties props;
		props.loadFromFile(configPath);
		port = props.getIntegerProperty("listen.port");
		string secureString = props.getProperty("secure");
		secure = secureString.empty() == false && !secureString.compare("y");
		certPath = props.getProperty("cert.path");
		keyPath = props.getProperty("key.path");
		defaultBrowsePath = props.getProperty("default.browse.path");
	}
	int getPort() {return port;}
	bool isSecure() {return secure;}
	string getCertPath() {return certPath;}
	string getKeyPath() {return keyPath;}
	string getDefaultBrowsePath() {return defaultBrowsePath;}
};


int main(int argc, char * args[]) {

	ServerConfig config;
	
	if (argc > 1) {
		config.load(args[1]);
    } else {
        char buffer[1024] = {0,};
        printf("Configuration file path: ");
        if (readline(buffer, sizeof(buffer)) > 0) {
			config.load(buffer);
        }
    }
    
	System::getInstance()->ignoreSigpipe();

    AnotherHttpServer * server = NULL;
    
    if (config.isSecure()) {
        class SecureServerSocketMaker : public ServerSocketMaker {
        private:
            string certPath;
            string keyPath;
        public:
            SecureServerSocketMaker(string certPath, string keyPath) :
				certPath(certPath), keyPath(keyPath) {}
            virtual ~SecureServerSocketMaker() {}
            virtual ServerSocket * makeServerSocket(int port) {
                SecureServerSocket * ret = new SecureServerSocket(port);
                ret->loadCert(certPath, keyPath);
                return ret;
            }
        };
        server = new AnotherHttpServer(config.getPort(),
									   new SecureServerSocketMaker(config.getCertPath(),
																   config.getKeyPath()));
    } else {
        server = new AnotherHttpServer(config.getPort());
    }

	FileBrowseHttpRequestHandler browse(config.getDefaultBrowsePath());
	server->registerRequestHandler("/browse", &browse);
	FileDownloadHttpRequestHandler file;
	server->registerRequestHandler("/file", &file);

    printf("Listening... %d\n", config.getPort());
    
	server->startAsync();

	while (1) {
		char buffer[1024] = {0,};
		if (readline(buffer, sizeof(buffer)) > 0) {
			if (!strcmp(buffer, "q")) {
				break;
			} else if (!strcmp(buffer, "s")) {
				printf("Listen port: %d\n", config.getPort());
			}
		}
	}

	server->stop();
    delete server;
    
    return 0;
}

