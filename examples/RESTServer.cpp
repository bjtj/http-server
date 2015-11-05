#include <iostream>
#include <libhttp-server/HttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class REST : public OnHttpRequestHandler {
public:
    REST() {
    }
    virtual ~REST() {
    }
    
    virtual void onRequest(HttpRequest & request, HttpResponse & response) {
        
        if (!request.remaining()) {
            string method = request.getMethod();
            if (Text::equalsIgnoreCase(method, "GET")) {
                response.write("GET");
            }
            if (Text::equalsIgnoreCase(method, "POST")) {
                response.write("POST");
            }
            if (Text::equalsIgnoreCase(method, "PUT")) {
                response.write("PUT");
            }
            if (Text::equalsIgnoreCase(method, "DELETE")) {
                response.write("DELETE");
            }
            response.setComplete();
        }
    }
};

int main(int argc, char * args[]) {
    
    bool done = false;
    HttpServer server(8083);
    server.setUseThreadedMultiConnType(true);
    
    REST rest;
    server.vpath("/rest/app", &rest);
    
    server.start();
    
    cout << "start" << endl;
    
    while (!done) {
        server.poll(1000);
    }
    
    cout << "stop" << endl;
    
    server.stop();
    
    return 0;
}