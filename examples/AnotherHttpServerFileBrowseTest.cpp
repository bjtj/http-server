#include <liboslayer/os.hpp>
#include <liboslayer/SecureSocket.hpp>
#include <liboslayer/Utils.hpp>
#include <liboslayer/Properties.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/FileTransfer.hpp>
#include <libhttp-server/HttpEncoderDecoder.hpp>
#include <libhttp-server/HttpSessionManager.hpp>
#include <libhttp-server/Url.hpp>
#include <liboslayer/Lisp.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

const Logger & logger = LoggerFactory::getDefaultLogger();
HttpSessionManager sessionManager(30 * 60 * 1000);

class ServerConfig : public HttpServerConfig {
private:
    string host;
    int port;
    bool secure;
    string certPath;
    string keyPath;
    string defaultBrowsePath;
    string browseIndexPath;
    
public:
    
    ServerConfig() : port(0), secure(false), defaultBrowsePath(".") {}
    virtual ~ServerConfig() {}
    void load(const string & configPath) {
        loadFromFile(configPath);
        host = getProperty("domain.host");
        port = getIntegerProperty("listen.port");
        string secureString = getProperty("secure");
        secure = secureString.empty() == false && !secureString.compare("y");
        certPath = getProperty("cert.path");
        keyPath = getProperty("key.path");
        defaultBrowsePath = getProperty("default.browse.path");
        browseIndexPath = getProperty("browse.index");
    }
    string getHost() { return host; }
    int getPort() { return port; }
    bool isSecure() { return secure; }
    string getCertPath() { return certPath; }
    string getKeyPath() { return keyPath; }
    string getDefaultBrowsePath() { return defaultBrowsePath; }
    string getBrowseIndexPath() { return browseIndexPath; }
};

ServerConfig config;

class SessionTool {
private:
public:
    SessionTool() {}
    virtual ~SessionTool() {}

	static string getSessionId(HttpRequest & request) {
		string path = request.getPath();
		size_t f = 0;
		if ((f = path.find(";")) != string::npos) {
			return path.substr(f + 1);
		}
		return "";
	}

	static HttpSession & getSession(HttpRequest & request) {
		string sessionId = getSessionId(request);
		HttpSession & session = (sessionId.empty() || !sessionManager.hasSession(Text::toInt(sessionId))) ?
			sessionManager.createSession() : sessionManager.getSession(Text::toInt(sessionId));
		
		if (session.outdated()) {
			sessionManager.destroySession(session.getId());
			return sessionManager.createSession();
		}
		return session;
	}

	static string urlMan(const string & u, HttpSession & session) {
		size_t f = u.find("?");
		string path = (f == string::npos) ? u : u.substr(0, f);
		string rest = (f == string::npos) ? "" : u.substr(f);
		return path + ";" + Text::toString(session.getId()) + rest;
	}
};


static void redirect(ServerConfig & config, HttpRequest & request, HttpResponse & response, HttpSession & session, const string & uri) {
    
    HttpResponseHeader & header = response.getHeader();
    
    string host = config.getHost();
    string port = Text::toString(request.getLocalAddress().getPort());
    if (!request.getHeaderField("Host").empty()) {
        Url url("http://" + request.getHeaderField("Host"));
        host = url.getHost();
        port = url.getPort();
    }
    
    response.setStatusCode(302);
    header.setHeaderField("Location", (config.isSecure() ? "https://" : "http://") +
                          host + ":" + port + "/" +
                          SessionTool::urlMan(uri, session));
    response.setContentType("text/html");
    header.setConnection("close");
}

class LispPage {
private:
    LISP::Env global_env;
public:
    LispPage() {
        LISP::native(global_env);
    }
    virtual ~LispPage() {}
    LISP::Env & env() {return global_env;}
    void applyWeb(HttpSession & session) {
        applyWeb(global_env, session);
    }
    void applyWeb(LISP::Env & env, HttpSession & session) {
        class LispSession : public LISP::Procedure {
        private:
            HttpSession & session;
        public:
            LispSession(const string & name, HttpSession & session) :
            LISP::Procedure(name), session(session) {}
            virtual ~LispSession() {}
            virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {
                string url = args[0].toString();
                return LISP::text(SessionTool::urlMan(url, session));
            }
        };
        env["url"] = LISP::Var(UTIL::AutoRef<LISP::Procedure>(new LispSession("url", session)));
    }
    
    bool eval(LISP::Var & var, LISP::Env & env) {
        try {
            LISP::eval(var, env);
            return !env.quit();
        } catch (const char * e) {
            cout << "ERROR: " << e << endl;
            return false;
        } catch (string & e) {
            cout << "ERROR: " << e << endl;
            return false;
        }
    }
    
    bool compile(const string & line, LISP::Env & env) {
        LISP::Var var = LISP::parse(line);
        return eval(var, env);
    }
    string parseLispPage(const string & src) {
        return parseLispPage(global_env, src);
    }
    string parseLispPage(LISP::Env & env, const string & src) {
        
        size_t f = 0;
        size_t s = 0;
        while ((f = src.find("<%", f)) != string::npos) {
            
            if (f - s > 0) {
                string txt = src.substr(s, f - s);
                LISP::Var & content = env["*content*"];
                env["*content*"] = LISP::text(content.nil() ? txt : content.toString() + txt);
            }
            
            size_t e = src.find("%>", f);
            string code = src.substr(f + 2, e - (f + 2));
            
            if (*code.begin() == '=') {
                string line = Text::trim(code.substr(1));
                line = "(string-append *content* " + line + ")";
                compile(line, env);
            } else {
                vector<string> lines = Text::split(code, "\n");
                for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
                    string & line = *iter;
                    if (!line.empty()) {
                        if (!compile(line, env)) {
                            break;
                        }
                    }
                }
            }
            
            s = f = e + 2;
        }
        if (!env.quit() && s < src.length()) {
            string txt = src.substr(s);
            LISP::Var & content = env["*content*"];
            env["*content*"] = LISP::text(content.nil() ? txt : content.toString() + txt);
        }
        
        return env["*content*"].toString();
    }
};

class LoginHttpRequestHandler : public HttpRequestHandler {
private:
public:
	LoginHttpRequestHandler() {}
	virtual ~LoginHttpRequestHandler() {}

	virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		HttpSession & session = SessionTool::getSession(request);
		session.updateLastAccessTime();

		bool login = !session["login"].compare("yes");

		if (!login) {
			if (!request.getParameter("pass").compare("secret")) {
				login = true;
				session["login"] = "yes";
			}
		}

		string content;

		if (!login) {
			HttpResponseHeader & responseHeader = response.getHeader();
			response.setStatusCode(200, "OK");
			response.setContentType("text/html");
			responseHeader.setConnection("close");
			
            content.append("<!DOCTYPE html>");
            content.append("<html>");
            content.append("<head>");
            content.append("<title>browse</title>");
            content.append("<style type=\"text/css\">body {font-family: arial; font-size: 10pt;}</style>");
            content.append("<meta charset=\"UTF-8\">");
            content.append("</head>");
            content.append("<body>");
            content.append("<form method=\"GET\" action=\"" + SessionTool::urlMan("login", session) + "\">" +
						   "Password: <input type=\"password\" name=\"pass\"/></form>");
            content.append("</body>");
            content.append("</html>");
        } else {
            
            redirect(config, request, response, session, "browse");
            return;
        }

		setFixedTransfer(response, content);
	}
};

class FileBrowseHttpRequestHandler : public HttpRequestHandler {
private:
	string defaultPath;
    string browseIndexPath;
public:
	FileBrowseHttpRequestHandler(const string & defaultPath, const string & browseIndexPath) : defaultPath(defaultPath), browseIndexPath(browseIndexPath) {}
	virtual ~FileBrowseHttpRequestHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {

		HttpSession & session = SessionTool::getSession(request);
		session.updateLastAccessTime();
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/html");
		responseHeader.setConnection("close");

		string path = request.getParameter("path");
        if (path.empty()) {
			path = defaultPath;
		}
        
        logger.logd(Text::format("** Path: %s [%s:%d]", path.c_str(),
                                 request.getRemoteAddress().getHost().c_str(), request.getRemoteAddress().getPort()));
        
        if (!Text::startsWith(path, defaultPath) || path.find("..") != string::npos) {
            response.setStatusCode(403);
            setFixedTransfer(response, "Not permitted");
            return;
        }

		bool debug = !request.getParameter("debug").empty();
        bool login = !session["login"].compare("yes");

		path = HttpDecoder::decode(HttpDecoder::decode_plus(path));

		string content;
        
        if (!login) {
            
            redirect(config, request, response, session, "login");
            return;
            
        } else {
            
            if (!browseIndexPath.empty()) {
                File file(browseIndexPath);
                FileReader reader(file);
                LispPage page;
                page.applyWeb(session);
                page.env()["*path*"] = LISP::text(path);
                content = page.parseLispPage(reader.dumpAsString());
            } else {
                vector<File> files = File::list(path);
                for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
                    if (iter->getName() == "index.lsp") {
                        redirect(config, request, response, session, "file?path=" + File::mergePaths(path, iter->getName()));
                    }
                }
                content = renderDir(path, debug, session);
            }
        }

        setFixedTransfer(response, content);
    }
    
    string renderDir(const string & path, bool debug, HttpSession & session) {
        
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
            content.append("<p><a href=\"" + SessionTool::urlMan("browse", session) + "\">home</a></p>");
            content.append("<ul>");
            for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
                
                if (!filter(*iter)) {
                    continue;
                }
                
                content.append("<li>");
                if (iter->isDirectory()) {
                    content.append("<span style=\"display:inline-block;width:15px;\">D</span>");
                    content.append("<a href=\"" + SessionTool::urlMan("browse", session) + "?path=" +
								   File::mergePaths(path, iter->getName()) + "\">");
                    content.append(iter->getName());
                    content.append("</a>");
                } else {
                    content.append("<span style=\"display:inline-block;width:15px;\">&nbsp;</span>");
                    content.append("<a href=\"" + SessionTool::urlMan("file", session) + "?path=" +
								   File::mergePaths(path, iter->getName()) + "\">");
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
};

class FileDownloadHttpRequestHandler : public HttpRequestHandler {
private:
public:
	FileDownloadHttpRequestHandler() {}
	virtual ~FileDownloadHttpRequestHandler() {}

	virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {

		HttpSession & session = SessionTool::getSession(request);
		session.updateLastAccessTime();
        
        HttpResponseHeader & responseHeader = response.getHeader();

        response.setStatusCode(200, "OK");
        response.setContentType("text/plain");
		responseHeader.setConnection("close");

		bool login = !session["login"].compare("yes");

		if (!login) {
            
            redirect(config, request, response, session, "login");
            return;
        }

		string path = request.getParameter("path");
		if (path.empty()) {
			response.setStatusCode(404);
			setFixedTransfer(response, "File not found / path: " + path);
			return;
		}

		path = HttpDecoder::decode(HttpDecoder::decode_plus(path));
        logger.logd(Text::format("** Path: %s [%s:%d]", path.c_str(), request.getRemoteAddress().getHost().c_str(), request.getRemoteAddress().getPort()));

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
			response.getHeader().setHeaderField("Content-Disposition",
												"attachment; filename=\"" + file.getName() + "\"");
		}

		if (type == "Application/x-lisp") {
			FileReader reader(file);
            
            LispPage page;
            page.applyWeb(session);
            page.env()["*path*"] = LISP::text(path);
            string content = page.parseLispPage(reader.dumpAsString());
            
			response.setContentType("text/html");
			setFixedTransfer(response, content);
			return;
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
		types["lsp"] = "Application/x-lisp";

		for (map<string, string>::iterator iter = types.begin(); iter != types.end(); iter++) {
			if (Text::equalsIgnoreCase(ext, iter->first)) {
				return iter->second;
			}
		}

		return "";
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


int main(int argc, char * args[]) {
	
	if (argc > 1) {
		config.load(args[1]);
    } else {
        char buffer[1024] = {0,};
        printf("Configuration file path: ");
        readline(buffer, sizeof(buffer));
        config.load(buffer);
    }
    
	System::getInstance()->ignoreSigpipe();

    AnotherHttpServer * server = NULL;

	config.setProperty("thread.count", 20);
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
			virtual void releaseSocket(ServerSocket * sock) {
				delete sock;
			}
        };
        server = new AnotherHttpServer(config, new SecureServerSocketMaker(config.getCertPath(),
																		   config.getKeyPath()));
    } else {
        server = new AnotherHttpServer(config);
    }

	FileBrowseHttpRequestHandler browse(config.getDefaultBrowsePath(), config.getBrowseIndexPath());
	server->registerRequestHandler("/browse*", &browse);
	FileDownloadHttpRequestHandler file;
	server->registerRequestHandler("/file*", &file);
	LoginHttpRequestHandler login;
	server->registerRequestHandler("/login*", &login);

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

