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
#include <libhttp-server/WebServerUtil.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

/**
 * @brief dump response handler
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
		responseHeader = response.header();
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
    
public:
    ServerConfig() {/**/}
    virtual ~ServerConfig() {/**/}
    string getHost() { return getProperty("domain.host"); }
	void setHost(const string & host) { setProperty("domain.host", host); }
    int getPort() { return getIntegerProperty("listen.port"); }
	void setPort(int port) { setProperty("listen.port", port); }
    bool isSecure() { return (getProperty("secure").empty() == false && getProperty("secure").compare("y") == 0); }
	void setSecure(bool secure) { setProperty("secure", "y"); }
    string getCertPath() { return getProperty("cert.path"); }
	void setCertPath(const string & certPath) { setProperty("cert.path", certPath); }
    string getKeyPath() { return getProperty("key.path"); }
	void setKeyPath(const string & keyPath) { setProperty("key.path", keyPath); }
};

/**
 * @brief 
 */
static void redirect(ServerConfig & config, HttpRequest & request, HttpResponse & response, AutoRef<HttpSession> session, const string & uri) {
    
    HttpResponseHeader & header = response.header();
    
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
	response.setContentLength(0);
}

/**
 * @brief 
 */
class StaticHttpRequestHandler : public HttpRequestHandler, public WebServerUtil {
private:
	bool dedicated;
	ServerConfig config;
    string basePath;
	string prefix;
	string indexName;
	map<string, string> mimeTypes;
    map<string, string> lspMemCache;
public:
	StaticHttpRequestHandler(const ServerConfig & config, const string & prefix)
		: dedicated(false), config(config), prefix(prefix)
	{
		mimeTypes = MimeTypes::getMimeTypes();
		basePath = config["static.base.path"];
		indexName = config["index.name"];
	}

	StaticHttpRequestHandler(bool dedicated, const ServerConfig & config, const string & prefix)
		: dedicated(dedicated), config(config), prefix(prefix)
	{
		mimeTypes = MimeTypes::getMimeTypes();
		basePath = config["static.base.path"];
		indexName = config["dedicated.index.name"];
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

	/**
	 * handle web
	 */
	void doHandle(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		
		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}
		
		string path = request.getPath();
		path = path.substr(prefix.size());
		if (path.empty()) {
			path = "/";
		}
		string log = Text::format("| %s:%d | STATIC | %s '%s'",
								  request.getRemoteAddress().getHost().c_str(),
								  request.getRemoteAddress().getPort(),
								  request.getMethod().c_str(),
								  path.c_str());
		
		File file(File::mergePaths(basePath, path));
		if (dedicated) {
			file = File(File::mergePaths(basePath, indexName));
		}
        if (!file.exists() || !file.isFile()) {
			if (path != "/") {
				logger->logd(log + " | 404 Not Found (" + file.getPath() + ")");
				setErrorPage(response, 404);
				return;
			}
			file = File(File::mergePaths(basePath, indexName));
			if (!file.exists()) {
				logger->logd(log + " | 404 No Index File, " + File::mergePaths(basePath, indexName));
				setErrorPage(response, 404);
				return;
			}
        }
		
		if (file.getExtension() == "lsp") {
            handleLispPage(file, request, sink, response);
			logger->logd(log + " : (LISP PAGE '" + file.getName() + "') | " + Text::toString(response.getStatusCode()));
			return;
		}
		
		logger->logd(log + " | 200 OK");
        response.setStatus(200);
		setContentTypeWithFile(request, response, file);
		if (request.getParameter("transfer") == "download") {
			setContentDispositionWithFile(request, response, file);
		}
        setFileTransfer(response, file);
    }

	/**
	 * handle lisp page
	 */
	void handleLispPage(File & file, HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		AutoRef<HttpSession> session = HttpSessionTool::getSession(request, sessionManager);
		session->updateLastAccessTime();
		response.setStatus(200);
		response.setContentType("text/html");
		if (lspMemCache.find(file.getPath()) == lspMemCache.end() ||
			(filesize_t)lspMemCache[file.getPath()].size() != file.getSize()) {
			FileStream reader(file, "rb");
			lspMemCache[file.getPath()] = reader.readFullAsString();
			reader.close();
		}
		string dump = lspMemCache[file.getPath()];
		string content = procLispPage(request, response, session, dump);
		if (response.redirectRequested()) {
			redirect(config, request, response, session, response.getRedirectLocation());
			return;
		}
		if (response.forwardRequested()) {
			string location = response.getForwardLocation();
			logger->logd(Text::format(" ** Forward :: location : %s", location.c_str()));
			request.setPath(location);
			response.cancelForward();
			doHandle(request, sink, response);
			return;
		}
		if (response["set-file-transfer"].empty() == false) {
			File file(response["set-file-transfer"]);
			if (!file.exists() || !file.isFile()) {
				setErrorPage(response, 404);
				return;
			}
			setFileTransferX(request, response, file);
			return;
		}
		setFixedTransfer(response, content);
	}

	/**
	 * proc lisp page
	 */
	string procLispPage(HttpRequest & request, HttpResponse & response, AutoRef<HttpSession> session, const string & dump) {
		LispPage page;
		page.applyWeb();
		page.applyAuth(request, response);
		page.applySession(session);
		page.applyRequest(request);
		page.applyResponse(response);
		unsigned long tick = tick_milli();
		string content = page.parseLispPage(dump);
		logger->logd(Text::format(" ** parsing : %ld ms.", tick_milli() - tick));
		return content;
	}

	/**
	 * error page
	 */
	void setErrorPage(HttpResponse & response, int errorCode) {
		response.setStatus(errorCode);
		switch (errorCode) {
		case 404:
			setFixedTransfer(response, "Not Found");
			break;
		default:
			break;
		}
	}

	/**
	 * set file transfer
	 */
	void setFileTransferX(HttpRequest & request, HttpResponse & response, File & file) {
		setContentTypeWithFile(request, response, file);
		setContentDispositionWithFile(request, response, file);
		string range = request.getHeaderFieldIgnoreCase("Range");
		if (!range.empty()) {
			try {
				setPartialFileTransfer(request, response, file);
			} catch (Exception e) {
				response.setContentType("text/html");
				response.setStatus(500);
				setFixedTransfer(response, e.getMessage());
			}
			return;
		}
		response.setStatus(200);
		setFileTransfer(response, file);
	}

	/**
	 * set content type
	 */
	void setContentTypeWithFile(HttpRequest & request, HttpResponse & response, File & file) {
		map<string, string> types = mimeTypes;

		if (types.find(file.getExtension()) != types.end()) {
			response.setContentType(types[file.getExtension()]);
		} else {
			response.setContentType("Application/octet-stream");
		}
	}

	/**
	 * set cotnent disposition
	 */
	void setContentDispositionWithFile(HttpRequest & request, HttpResponse & response, File & file) {
		response.setHeaderField("Content-Disposition",
										 "attachment; filename=\"" + file.getFileName() + "\"");
	}
};

/**
 * @brief 
 */
class ProxyHandler : public HttpRequestHandler, public WebServerUtil {
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
class AuthHttpRequestHandler : public HttpRequestHandler, public WebServerUtil {
private:
	AutoRef<BasicAuth> auth;
public:
	AuthHttpRequestHandler(AutoRef<BasicAuth> auth) : auth(auth) {}
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
		logger->logd(Text::format("** Part2: %s [%s:%d]", request.getRawPath().c_str(),
                                 request.getRemoteAddress().getHost().c_str(),
								 request.getRemoteAddress().getPort()));
		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}
		response.setStatus(200);
		setFixedTransfer(response, "hello");
    }
};

string readline() {
	FileStream fs(stdin);
	return fs.readline();
}

string prompt(const char * msg) {
    printf("%s", msg);
	return readline();
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

	System::getInstance()->ignoreSigpipe();
	
	LoggerFactory::getInstance().setLoggerDescriptorSimple("*", "basic", "console");
	MimeTypes::load(DATA_PATH"/mimetypes");
	
	string configPath;
	if (argc > 1) {
		configPath = args[1];
    } else {
        printf("Configuration file path(empty -> default): ");
		configPath = readline();
    }

	ServerConfig config;
	config["domain.host"] = "localhost";
	config["listen.port"] = "9000";
	config["static.base.path"] = "./";
	config["index.name"] = "index.html";
    config.setProperty("thread.count", 50);
	if (configPath.empty() == false) {
		config.loadFromFile(configPath);
	}

    AnotherHttpServer * server = NULL;
    if (config.isSecure() == false) {
		server = new AnotherHttpServer(config);
	} else {
#if !defined(USE_OPENSSL)
		throw Exception("SSL not supported");
#else
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
#endif
    }

	AutoRef<HttpRequestHandler> staticHandler(new StaticHttpRequestHandler(config, "/static"));
    server->registerRequestHandler("/static*", staticHandler);

	AutoRef<HttpRequestHandler> davStaticHandler(new StaticHttpRequestHandler(true, config, "/dav"));
    server->registerRequestHandler("/dav*", davStaticHandler);

	AutoRef<BasicAuth> auth(new BasicAuth(config["auth.username"], config["auth.password"]));
	AutoRef<HttpRequestHandler> authHandler(new AuthHttpRequestHandler(auth));
	server->registerRequestHandler("/auth*", authHandler);
	
	AutoRef<HttpRequestHandler> proxyHandler(new ProxyHandler(config));
	server->registerRequestHandler("/proxy", proxyHandler);

	server->startAsync();

	printf("[Listening... / port: %d]\n", config.getPort());
	
	while (1) {
		string line = readline();
		if (line.empty() == false) {
			if (line == "q" || line == "quit") {
				break;
			} else if (line == "s") {
				printf("Listen port: %d\n", config.getPort());
				printf(" * Connections: %zu\n", server->connections());
                vector<AutoRef<Connection> > conns = server->connectionManager().getConnectionList();
                for (vector<AutoRef<Connection> >::iterator iter = conns.begin(); iter != conns.end(); iter++) {
                    printf("  - Recv: %10s Bytes / Send: %10s Bytes (%ld)\n",
						   Text::toCommaNumber(Text::toString((*iter)->recvCount())).c_str(),
						   Text::toCommaNumber(Text::toString((*iter)->sendCount())).c_str(),
						   (*iter)->sendTryCount());
                }
			} else if (line == "load") {
				// TODO: implement
			}
		}
	}

	printf("[Stopping...]\n");
	server->stop();
    delete server;
	printf("[Done]\n");
    
    return 0;
}

