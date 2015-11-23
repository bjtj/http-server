#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/ChunkedTransfer.hpp>
#include <libhttp-server/FixedTransfer.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class ResponseHandler : public OnResponseListener {
private:
public:
    ResponseHandler() {
    }
    virtual ~ResponseHandler() {
    }
    virtual void onResponseHeader(HttpResponse & response) {
        
        cout << response.getHeader().toString() << endl;
        
        if (response.getHeader().isChunkedTransfer()) {
            ChunkedTransfer * transfer = new ChunkedTransfer;
            response.setTransfer(transfer);
        } else if (response.getHeader().getContentLength() > 0) {
            FixedTransfer * transfer = new FixedTransfer(response.getHeader().getContentLength());
            response.setTransfer(transfer);
        } else {
            // do nothing
        }
    }
    virtual void onTransferDone(DataTransfer * transfer) {
        if (transfer) {
            string content = transfer->getString();
            cout << content << endl;
        }
    }
    virtual void onError(Exception & e) {
        cout << "Error/e: " << e.getMessage() << endl;
    }
};

void s_test() {
    //    Url url("http://www.google.com");
    //    AnotherHttpClient client(url);
    AnotherHttpClient client;
    
    ResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    
//    client.setUrl("http://www.google.com");
    client.setUrl("http://192.168.0.22:32469/DeviceDescription.xml");
    client.setRequest("GET", LinkedStringMap(), NULL);
    client.execute();
}

void s_test_reuse() {
    AnotherHttpClient client;
    
    ResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    
    client.setUrl("http://www.google.com");
    client.setRequest("GET", LinkedStringMap(), NULL);
    client.execute();
    
    cout << " --------------------------------------- " << endl;
    
    client.setUrl("http://www.example.com");
    client.setRequest("GET", LinkedStringMap(), NULL);
    client.execute();
    
    cout << " --------------------------------------- " << endl;
    
    client.setUrl("http://www.naver.com");
    client.setRequest("GET", LinkedStringMap(), NULL);
    client.execute();
}

void s_post() {

	AnotherHttpClient client;
    
    ResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    
    client.setUrl("http://httpbin.org/post");
    client.setRequest("POST", LinkedStringMap(), new FixedTransfer("hello", 5));
    client.execute();

}

int main(int argc, char * args[]) {
    s_test();
    //s_test_reuse();
//	s_post();

	getchar();
    
    return 0;
}