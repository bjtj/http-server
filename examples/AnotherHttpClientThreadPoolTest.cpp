#include <iostream>
#include <liboslayer/Text.hpp>
#include <libhttp-server/AnotherHttpClientThreadPool.hpp>
#include <libhttp-server/ChunkedTransfer.hpp>
#include <libhttp-server/FixedTransfer.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

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
    
    AnotherHttpClientThreadPool pool(1);
    
    SampleOnRequestCompleteListener listener;
    pool.setOnRequestCompleteListener(&listener);
    
    pool.start();
    
    while (1) {
        char buffer[1024] = {0,};
        if (readline(buffer, sizeof(buffer))) {

			pool.collectUnflaggedThreads();

            if (!strcmp(buffer, "q")) {
                break;
            }

			if (!strcmp(buffer, "e")) {
				pool.setRequest(Url("http://example.com"), "GET", NULL, NULL);
			}

			if (Text::startsWith(buffer, "get ")) {
				string url(buffer);
                pool.setRequest(Url(url.substr(4)), "GET", NULL, NULL);
            }

			if (Text::startsWith(buffer, "post ")) {
				string content = "a=a1&b=b1";
                pool.setRequest(Url(buffer + 5), "POST", AutoRef<DataTransfer>(new FixedTransfer(content.c_str(), content.length())), NULL);
            }
        }
    }
    
    pool.stop();
    
    return 0;
}