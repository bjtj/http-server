#include <iostream>
#include <libhttp-server/HttpServer.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class Hello : public OnHttpRequestHandler {
public:
    Hello() {
    }
    virtual ~Hello() {
    }
    
    virtual void onHttpRequest(HttpRequest & request, HttpResponse & response) {
        
        int len = request.getContentLength();
        ChunkedBuffer & buffer = request.getChunkedBuffer();
        if (len != buffer.getChunkSize()) {
            buffer.setChunkSize(len);
        }
        
        char readBuffer[1024] = {0,};
        int readLen = request.getSocket().recv(readBuffer, buffer.getReadSize(sizeof(readBuffer)));
        buffer.readChunkData(readBuffer, readLen);
        
        if (!buffer.remainingDataBuffer()) {
            
            cout << request.getHeader().toString() << endl;
            cout << string(buffer.getChunkData(), buffer.getChunkSize()) << endl;
            
            string path = request.getPath();
            response.write("hello world - " + path);
            response.setComplete();
        }
    }
};

size_t readline(char * buffer, size_t max) {
    fgets(buffer, (int)max - 1, stdin);
    buffer[strlen(buffer) - 1] = 0;
    return strlen(buffer);
}

int main(int argc, char * args[]) {
    
    int port = 8083;
    
    bool done = false;
    HttpServer server(port);
    server.setUseThreadedMultiConnType(true);
    
    Hello hello;
    server.vpath("/hello", &hello);
    
    server.startAsync();
    
    cout << "start/port: " << port << endl;
    
    while (!done) {
        char buffer[1024] = {0,};
        if (readline(buffer, sizeof(buffer)) > 0) {
            if (!strcmp(buffer, "q")) {
                done = true;
                break;
            }
            if (!strcmp(buffer, "get")) {
                HttpClient<int> client;
                client.request("http://127.0.0.1:8083/hello", 0);
            }
            
            if (!strcmp(buffer, "post")) {
                HttpClient<int> client;
                const char * data = "hello world!";
                client.request("http://127.0.0.1:8083/hello", "POST", data, strlen(data), 0);
            }
        }
    }
    
    cout << "stop" << endl;
    
    server.stop();
    
    return 0;
    
    return 0;
}