#include <liboslayer/os.hpp>
#include <liboslayer/SecureSocket.hpp>
#include <liboslayer/Utils.hpp>
#include <liboslayer/Properties.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/FileTransfer.hpp>
#include <libhttp-server/HttpSessionManager.hpp>
#include <libhttp-server/HttpSessionTool.hpp>
#include <libhttp-server/LispPage.hpp>
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
    
public:
    
    ServerConfig() : port(0) {}
    virtual ~ServerConfig() {}
    void load(const string & configPath) {
        loadFromFile(configPath);
        host = getProperty("domain.host");
        port = getIntegerProperty("listen.port");
    }
    string getHost() { return host; }
    int getPort() { return port; }
};

ServerConfig config;

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
    header.setHeaderField("Location", "http://" +
                          host + ":" + port + "/" +
                          HttpSessionTool::urlMan(uri, session));
    response.setContentType("text/html");
    header.setConnection("close");
}
        
class SinglePageHttpRequestHandler : public HttpRequestHandler {
private:
	string path;
public:
	SinglePageHttpRequestHandler(const string & path) : path(path) {}
	virtual ~SinglePageHttpRequestHandler() {}

	virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {

		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}

		HttpSession & session = HttpSessionTool::getSession(sessionManager, request);
		session.updateLastAccessTime();
		
		File file(path);
		FileReader reader(file);

		response.setStatusCode(200);
		response.setContentType("text/html");
            
		LispPage page;
		page.applyWeb();
		page.applySession(session);
		page.applyRequest(request);
		page.applyResponse(response);
		page.applyLoadPage();
		string content = page.parseLispPage(reader.dumpAsString());
		
		setFixedTransfer(response, content);
	}
};

string readline() {
	char buffer[1024] = {0,};
	if (fgets(buffer, sizeof(buffer) - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return string(buffer);
	}
	return "";
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


int main(int argc, char * args[]) {
	
	if (argc > 1) {
		config.load(args[1]);
    } else {
        printf("** Configuration file path: ");
        config.load(readline());
    }
    
	System::getInstance()->ignoreSigpipe();

    AnotherHttpServer * server = NULL;

	config.setProperty("thread.count", 20);
	server = new AnotherHttpServer(config);

	AutoRef<HttpRequestHandler> single(new SinglePageHttpRequestHandler(config["default.page"]));
	server->registerRequestHandler("/", single);
	server->registerRequestHandler("/index.htm", single);

    printf("** Listening... %d\n", config.getPort());
    
	server->startAsync();

	while (1) {
		string line;
		if (!(line = readline()).empty()) {
			if (line == "q") {
				break;
			} else if (line == "s") {
				printf("** Listen port: %d\n", config.getPort());
			} else if (line == "apps") {
				// TODO: app list
			} else if (line == "load") {
				// TODO: load app -> url -> path
				string uri = prompt("** Mapping uri: ");
				HttpServerConfig config;
				config.loadFromFile(prompt("** Properties file: "));

				server->registerRequestHandler(uri, AutoRef<HttpRequestHandler>(new SinglePageHttpRequestHandler(config["default.page"])));
			} else if (line == "unload") {
				// TODO: unload app -> url
				string name = prompt("** Name: ");
			}
		}
	}

	printf("** Stopping\n");
	server->stop();
    delete server;
	printf("** Done\n");
    
    return 0;
}

