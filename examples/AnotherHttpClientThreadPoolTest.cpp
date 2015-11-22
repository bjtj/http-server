#include <iostream>
#include <libhttp-server/AnotherHttpClientThreadPool.hpp>
#include <libhttp-server/ChunkedTransfer.hpp>
#include <libhttp-server/FixedTransfer.hpp>

using namespace std;
using namespace HTTP;


class SampleOnRequestCompleteListener : public OnRequestCompleteListener {
private:
public:
    SampleOnRequestCompleteListener() {
    }
    virtual ~SampleOnRequestCompleteListener() {
    }
    
    virtual void onRequestComplete(Url & url, HttpResponse & response, const string & content, UserData * userData) {
        
        if (!response.getHeader().isRedirection()) {
            
            cout << "url : " << url.toString() << endl;
            cout << content << endl;
        }
    }
    virtual void onRequestError(Url & url, UserData * userData) {
        cout << "Error/url: " << url.toString() << endl;
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
    
    AnotherHttpClientThreadPool pool(10);
    
    SampleOnRequestCompleteListener listener;
    pool.setOnRequestCompleteListener(&listener);
    
    pool.start();
    
    while (1) {
        char buffer[1024] = {0,};
        if (readline(buffer, sizeof(buffer))) {
            if (!strcmp(buffer, "q")) {
                break;
            } else {
                pool.setRequest(Url(buffer), "GET", NULL, NULL);
            }
        }
    }
    
    pool.stop();
    
    return 0;
}