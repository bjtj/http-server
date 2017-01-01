#include <liboslayer/os.hpp>
#include <liboslayer/SecureSocket.hpp>
#include <liboslayer/FileStream.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/Properties.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/HttpSessionManager.hpp>
#include <libhttp-server/HttpSessionTool.hpp>
#include <libhttp-server/LispPage.hpp>
#include <libhttp-server/Url.hpp>
#include <libhttp-server/MimeTypes.hpp>
#include <libhttp-server/StringDataSink.hpp>
#include <liboslayer/Lisp.hpp>
#include <liboslayer/Base64.hpp>
#include <liboslayer/Hash.hpp>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/BasicAuth.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static AutoRef<Logger> logger = LoggerFactory::getInstance().getLogger(__FILE__);


/**
 * @brief 
 */
class DumpResponseHandler : public OnHttpResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
public:
    DumpResponseHandler() {}
    virtual ~DumpResponseHandler() {}
	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}
	virtual void onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
		responseHeader = response.getHeader();
        if (!sink.nil()) {
			dump = ((StringDataSink*)&sink)->data();
        } else {
			throw Exception("error");
		}
    }
    virtual void onError(OS::Exception & e, AutoRef<UserData> userData) {
		logger->loge("Error/e: " + e.getMessage());
    }
	HttpResponseHeader & getResponseHeader() {
		return responseHeader;
	}
	string & getDump() {
		return dump;
	}
};

HttpSessionManager sessionManager(30 * 60 * 1000);

/**
 * @brief 
 */
class ServerConfig : public HttpServerConfig {
private:
    string host;
    int port;
    bool secure;
    string certPath;
    string keyPath;
    
public:
    ServerConfig() : port(0), secure(false) {}
    virtual ~ServerConfig() {}
    void load(const string & configPath) {
        loadFromFile(configPath);
        host = getProperty("domain.host");
        port = getIntegerProperty("listen.port");
        string secureString = getProperty("secure");
        secure = secureString.empty() == false && !secureString.compare("y");
        certPath = getProperty("cert.path");
        keyPath = getProperty("key.path");
    }
    string getHost() { return host; }
    int getPort() { return port; }
    bool isSecure() { return secure; }
    string getCertPath() { return certPath; }
    string getKeyPath() { return keyPath; }
};

/**
 * @brief 
 */
static void redirect(ServerConfig & config, HttpRequest & request, HttpResponse & response, HttpSession & session, const string & uri) {
    
    HttpResponseHeader & header = response.getHeader();
    
    string host = config.getHost();
    string port = Text::toString(request.getLocalAddress().getPort());
    if (!request.getHeaderField("Host").empty()) {
        Url url("http://" + request.getHeaderField("Host"));
        host = url.getHost();
        port = url.getPort();
    }
    
    response.setStatus(302);
    header.setHeaderField("Location", (config.isSecure() ? "https://" : "http://") +
                          host + ":" + port + "/" +
                          HttpSessionTool::urlMan(uri, session));
    response.setContentType("text/html");
}

/**
 * @brief 
 */
class StaticHttpRequestHandler : public HttpRequestHandler {
private:
	ServerConfig & config;
	LISP::Env & env;
    string basePath;
	string prefix;
	string indexName;
	map<string, string> mimeTypes;
    map<string, string> lspMemCache;
public:
	StaticHttpRequestHandler(ServerConfig & config,
							 LISP::Env & env,
							 const string & basePath,
							 const string & prefix,
							 const string & indexName)
		: config(config), env(env), basePath(basePath), prefix(prefix), indexName(indexName)
	{
		mimeTypes = MimeTypes::getMimeTypes();
	}
    virtual ~StaticHttpRequestHandler() {/* empty */}
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		try {
			doHandle(request, sink, response);
			if (request.getMethod() == "HEAD") {
				response.setTransfer(AutoRef<DataTransfer>(NULL));
			}
		} catch (Exception & e) {
			logger->loge(" ** error");
			response.setStatus(500);
			response.setContentType("text/html");
			setFixedTransfer(response, "Server Error/" + e.getMessage());
		}
	}

	void doHandle(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		string log;
		logger->logd(Text::format("** Part2: %s [%s:%d]", request.getHeader().getPart2().c_str(),
                                 request.getRemoteAddress().getHost().c_str(),
								 request.getRemoteAddress().getPort()));
		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}
        response.setContentLength(0);
		HttpSession & session = HttpSessionTool::getSession(request, sessionManager);
		session.updateLastAccessTime();
		string path = request.getPath();
		path = path.substr(prefix.size());
		log = "** static :: prefix : '" + prefix + "' / path : '" + path + "'";
        File file(File::mergePaths(basePath, path));
        if (!file.exists() || !file.isFile()) {
			if (path == "/") {
				file = File(File::mergePaths(basePath, indexName));
				if (!file.exists()) {
					logger->logd(log + " := 404 no index, " + File::mergePaths(basePath, indexName));
					response.setStatus(404);
					return;
				}
			} else {
				logger->logd(log + " := 404 not found (" + file.getPath() + ")");
				response.setStatus(404);
				return;
			}
        }

		if (file.getExtension() == "lsp") {
			response.setStatus(200);
			response.setContentType("text/html");
            if (lspMemCache.find(file.getPath()) == lspMemCache.end() ||
                (filesize_t)lspMemCache[file.getPath()].size() != file.getSize()) {
                FileStream reader(file, "rb");
                lspMemCache[file.getPath()] = reader.readFullAsString();
                reader.close();
            }
            string dump = lspMemCache[file.getPath()];
			LispPage page(&env);
			page.applyWeb();
			page.applyAuth(request, response);
			page.applySession(session);
			page.applyRequest(request);
			page.applyResponse(response);
			string content = page.parseLispPage(dump);
			env.gc();
			if (response.needRedirect()) {
				logger->logd(log + " := 302 redirect");
				redirect(config, request, response, session, response.getRedirectLocation());
				return;
			}
			if (response["set-file-transfer"].empty() == false) {
				File file(response["set-file-transfer"]);
				if (!file.exists() || !file.isFile()) {
					logger->logd(log + " := 404");
					response.setStatus(404);
					setFixedTransfer(response, "Not Found");
					return;
				}
				setContentTypeWithFile(request, response, file);
				setContentDispositionWithFile(request, response, file);
				string range = request.getHeaderFieldIgnoreCase("Range");
				if (!range.empty()) {
					size_t f = range.find("=");
					if (f != string::npos) {
						range = range.substr(f + 1);
						f = range.find("-");
						if (f != string::npos) {
							string start = range.substr(0, f);
							string end = range.substr(f + 1);
							try {
								logger->logd(log + " := 206 partial transfer(" + start + "~" + end + ")");
								// TODO: implicitly handling the response code
								setPartialFileTransfer(response,
													   file,
													   (size_t)Text::toLong(start),
													   (size_t)Text::toLong(end));
								return;
							} catch (Exception e) {
								logger->logd(log + " := 500 " + e.getMessage());
								response.setContentType("text/html");
								response.setStatus(500);
								setFixedTransfer(response, e.getMessage());
								return;
							}
						}
					}
				}
				logger->logd(log + " := 200 fixed transfer");
				response.setStatus(200);
				setFileTransfer(response, file);
				return;
			}
			logger->logd(log + " := 200 lisp page");
			setFixedTransfer(response, content);
			return;
		}
		logger->logd(log + " := 200 static");
        response.setStatus(200);
		setContentTypeWithFile(request, response, file);
		if (request.getParameter("transfer") == "download") {
			setContentDispositionWithFile(request, response, file);
		}
        setFileTransfer(response, file);
    }

	void setContentTypeWithFile(HttpRequest & request, HttpResponse & response, File & file) {
		map<string, string> types = mimeTypes;

		if (types.find(file.getExtension()) != types.end()) {
			response.setContentType(types[file.getExtension()]);
		} else {
			response.setContentType("Application/octet-stream");
		}
	}

	void setContentDispositionWithFile(HttpRequest & request, HttpResponse & response, File & file) {
		response.getHeader().setHeaderField("Content-Disposition",
											"attachment; filename=\"" + file.getFileName() + "\"");
	}
};

/**
 * @brief 
 */
class ProxyHandler : public HttpRequestHandler {
private:

	/**
	 * @brief 
	 */
	class DumpResult {
	private:
		int _statusCode;
		string _contentType;
		string _dump;
	public:
		DumpResult() : _statusCode(0) {}
		virtual ~DumpResult() {}
		int & statusCode() {
			return _statusCode;
		}
		string & contentType() {
			return _contentType;
		}
		string & dump() {
			return _dump;
		}
	};

	/**
	 * @brief 
	 */
	class Maker : public SocketMaker {
	public:
		Maker() {}
		virtual ~Maker() {}
		virtual AutoRef<Socket> make(const string & protocol, const InetAddress & addr) {
			if (protocol == "https") {
#if defined(USE_OPENSSL)
				class MyVerifier : public CertificateVerifier {
				public:
					MyVerifier() {}
					virtual ~MyVerifier() {}
					virtual bool onVerify(const VerifyError & err, const Certificate & cert) {
						return true;
					}
				};

				SecureSocket * sock = new SecureSocket(addr);
				sock->setVerifier(AutoRef<CertificateVerifier>(new MyVerifier));
				return AutoRef<Socket>(sock);
#else
				throw Exception("openssl not supported");
#endif
			}
			return AutoRef<Socket>(new Socket(addr));
		}
	};

	ServerConfig config;
	SimpleHash hash;
	
public:
	ProxyHandler(const ServerConfig & config) : config(config) {}
    virtual ~ProxyHandler() {}
    
    virtual AutoRef<DataSink> getDataSink() {
        return AutoRef<DataSink>(new StringDataSink);
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

		string url = request.getParameter("u");

		if (config["proxy.cache"] == "y") {
			
			unsigned long h = hash.hash(url.c_str());
			string path = config["proxy.cache.path"];
			if (!path.empty()) {
				File p(path);
				p.mkdir();
				path = File::mergePaths(path, Text::toString(h));
			} else {
				path = Text::toString(h);
			}

			File file(path);
			if (file.exists()) {

				FileStream in(file, "rb");
				int statusCode = Text::toInt(in.readline());
				string contentType = in.readline();
				string dump = in.readFullAsString();
				in.close();
				
				response.setStatus(statusCode);
				response.setContentType(contentType);
				setFixedTransfer(response, dump);
				
			} else {

				DumpResult result = getHttpDump(request.getMethod(), url);
				response.setStatus(result.statusCode());
				if (result.statusCode() != 500) {

					FileStream out(file, "wb");
					out.writeline(Text::toString(result.statusCode()));
					out.writeline(result.contentType());
					out.write(result.dump());
					out.close();
					
					response.setContentType(result.contentType());
					setFixedTransfer(response, result.dump());
				}
			}
			
		} else {

			DumpResult result = getHttpDump(request.getMethod(), url);
			response.setStatus(result.statusCode());
			if (result.statusCode() != 500) {
				response.setContentType(result.contentType());
				setFixedTransfer(response, result.dump());
			}
		}
	}

	DumpResult getHttpDump(const string & method, const string & url) {

		DumpResult result;
		DumpResponseHandler handler;
		AnotherHttpClient client(AutoRef<SocketMaker>(new Maker));
		client.setOnHttpResponseListener(&handler);
		client.setConnectionTimeout(3000);
		client.setRecvTimeout(3000);
		client.setFollowRedirect(true);
		client.setUrl(url);
		client.setRequest(method, LinkedStringMap());
		try {
			client.execute();
			result.statusCode() = handler.getResponseHeader().getStatusCode();
			result.contentType() = handler.getResponseHeader()["content-type"];
			result.dump() = handler.getDump();
		} catch (Exception & e) {
			logger->loge(e.getMessage());
			result.statusCode() = 500;
		}
		return result;
	}
};

/**
 * @brief 
 */
class AuthHttpRequestHandler : public HttpRequestHandler {
private:
	AutoRef<BasicAuth> auth;
public:
	AuthHttpRequestHandler(AutoRef<BasicAuth> auth) : auth(auth) {
	}
    virtual ~AuthHttpRequestHandler() {}
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

		try {
			if (!auth.nil() && !auth->validate(request)) {
				auth->setAuthentication(response);
				response.setContentType("text/plain");
				setFixedTransfer(response, "Authentication required");
				return;
			}
			
			doHandle(request, sink, response);

			if (request.getMethod() == "HEAD") {
				response.setTransfer(AutoRef<DataTransfer>(NULL));
			}
			
		} catch (Exception & e) {
			logger->loge(" ** error");
			response.setStatus(500);
			response.setContentType("text/html");
			setFixedTransfer(response, "Server Error/" + e.getMessage());
		}
	}

	void doHandle(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

		string log;

		logger->logd(Text::format("** Part2: %s [%s:%d]", request.getHeader().getPart2().c_str(),
                                 request.getRemoteAddress().getHost().c_str(),
								 request.getRemoteAddress().getPort()));

		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}

		response.setStatus(200);
		setFixedTransfer(response, "hello");
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

#if !defined(DATA_PATH)
#define DATA_PATH ""
#endif

/**
 * @brief 
 */
int main(int argc, char * args[]) {

	LoggerFactory::getInstance().setLoggerDescriptorSimple("*", "basic", "console");

	MimeTypes::load(DATA_PATH"/mimetypes");

	ServerConfig config;

	string configPath;
	
	if (argc > 1) {
		configPath = args[1];
    } else {
        char buffer[1024] = {0,};
        printf("Configuration file path: ");
        readline(buffer, sizeof(buffer));
		configPath = buffer;
    }

	config.load(configPath);
    
	System::getInstance()->ignoreSigpipe();

    AnotherHttpServer * server = NULL;

	config.setProperty("thread.count", 50);
    if (config.isSecure()) {
        
#if defined(USE_OPENSSL)
        
        logger->logd(" ** OpenSSL version: " + SecureContext::getOpenSSLVersion());
        
        class SecureServerSocketMaker : public ServerSocketMaker {
        private:
            string certPath;
            string keyPath;
        public:
            SecureServerSocketMaker(string certPath, string keyPath) :
				certPath(certPath), keyPath(keyPath) {}
            virtual ~SecureServerSocketMaker() {}
            virtual AutoRef<ServerSocket> makeServerSocket(int port) {
                AutoRef<ServerSocket> ret(new SecureServerSocket(port));
                ((SecureServerSocket*)&ret)->loadCert(certPath, keyPath);
                return ret;
            }
        };
		AutoRef<ServerSocketMaker> maker(new SecureServerSocketMaker(config.getCertPath(), config.getKeyPath()));
        server = new AnotherHttpServer(config, maker);
#else
        throw Exception("SSL not supported");
#endif
    } else {
        server = new AnotherHttpServer(config);
    }

	LISP::Env env;
	LISP::native(env);
	AutoRef<HttpRequestHandler> staticHandler(new StaticHttpRequestHandler(config,
																		   env,
																		   config["static.base.path"],
																		   "/static",
																		   config["index.name"]));
    server->registerRequestHandler("/static/*", staticHandler);

	AutoRef<BasicAuth> auth(new BasicAuth(config["auth.username"], config["auth.password"]));
	AutoRef<HttpRequestHandler> authHandler(new AuthHttpRequestHandler(auth));
	server->registerRequestHandler("/auth*", authHandler);
	
	AutoRef<HttpRequestHandler> proxyHandler(new ProxyHandler(config));
	server->registerRequestHandler("/proxy", proxyHandler);

	server->startAsync();

	printf("Listening... %d\n", config.getPort());
	
	while (1) {
		char buffer[1024] = {0,};
		if (readline(buffer, sizeof(buffer)) > 0) {
			if (!strcmp(buffer, "q") || !strcmp(buffer, "quit")) {
				break;
			} else if (!strcmp(buffer, "s")) {
				printf("Listen port: %d\n", config.getPort());
				printf(" * Connections: %ld\n", server->connections());
                vector<AutoRef<Connection> > conns = server->connectionManager().getConnectionList();
                for (vector<AutoRef<Connection> >::iterator iter = conns.begin(); iter != conns.end(); iter++) {
                    printf(" - recv: %ld, send: %ld (%ld)\n", (*iter)->recvCount(), (*iter)->sendCount(), (*iter)->sendTryCount());
                }
			} else if (!strcmp(buffer, "load")) {
				// TODO: implement
			}
		}
	}

	printf("** stopping\n");
	server->stop();
    delete server;
	printf("** done\n");
    
    return 0;
}

