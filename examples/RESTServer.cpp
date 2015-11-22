#include <iostream>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class REST : public HttpRequestHandler {
private:
public:
    REST() {}
    virtual ~REST() {}
    
    virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) {
        
        string method = request.getMethod();
        string content;
        if (Text::equalsIgnoreCase(method, "GET")) {
            content = "GET";
        }
        if (Text::equalsIgnoreCase(method, "POST")) {
            content = "POST";
        }
        if (Text::equalsIgnoreCase(method, "PUT")) {
            content = "PUT";
        }
        if (Text::equalsIgnoreCase(method, "DELETE")) {
            content = "DELETE";
        }
        
        setFixedTransfer(response, content);
    }
    
    virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
    }
};






int main(int argc, char * args[]) {
    
    bool done = false;
    AnotherHttpServer server(8083);
    
    REST rest;
    server.registerRequestHandler("/rest/app", &rest);
    
    server.start();
    
    cout << "start" << endl;
    
    while (!done) {
        server.poll(1000);
    }
    
    cout << "stop" << endl;
    
    server.stop();
    
    return 0;
}