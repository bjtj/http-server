#include <liboslayer/os.hpp>
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
using namespace HTTP;

class DumpHandler : public HttpRequestHandler {
private:
public:
    DumpHandler() {}
    virtual ~DumpHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        response.setStatusCode(200);
        response.setContentType("text/html");
        
        if (request.getMethod() == "GET") {
            setFixedTransfer(response, "<html><form method=\"post\"><textarea name=\"text\" type=\"text\"></textarea><input type=\"submit\"/></form></html>");
        }
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
        
        if (!request.getTransfer().nil()) {
            cout << HttpDecoder::decode(request.getTransfer()->getString()) << endl;
        }
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
    
    HttpServerConfig config;
    config["listen.port"] = "8888";
    System::getInstance()->ignoreSigpipe();
    
    AnotherHttpServer * server = NULL;
    
    config.setProperty("thread.count", 20);
    server = new AnotherHttpServer(config);
    
    DumpHandler handler;
    server->registerRequestHandler("/*", &handler);
    
    printf("Listening... %d\n", config.getIntegerProperty("listen.port"));
    
    server->startAsync();
    
    while (1) {
        char buffer[1024] = {0,};
        if (readline(buffer, sizeof(buffer)) > 0) {
            if (!strcmp(buffer, "q")) {
                break;
            }
        }
    }
    
    server->stop();
    delete server;
    
    return 0;
}