#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/ChunkedTransfer.hpp>
#include <libhttp-server/FixedTransfer.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

class DumpResponseHandler : public OnResponseListener {
private:
	string dump;
public:
    DumpResponseHandler() {
    }
    virtual ~DumpResponseHandler() {
    }
    virtual void onTransferDone(HttpResponse & response, DataTransfer * transfer, AutoRef<UserData> userData) {
        if (transfer) {
            dump = transfer->getString();
        }
    }
    virtual void onError(Exception & e, AutoRef<UserData> userData) {
        cout << "Error/e: " << e.getMessage() << endl;
    }
	string & getDump() {
		return dump;
	}
};

class ResponseHandler : public OnResponseListener {
private:
public:
    ResponseHandler() {
    }
    virtual ~ResponseHandler() {
    }
    virtual void onResponseHeader(HttpResponse & response, AutoRef<UserData> userData) {
        
        cout << response.getHeader().toString() << endl;
        response.setTransfer(createDataTransfer(response.getHeader()));
    }
    virtual void onTransferDone(HttpResponse & response, DataTransfer * transfer, AutoRef<UserData> userData) {
        if (transfer) {
            string content = transfer->getString();
            cout << content << endl;
        }
    }
    virtual void onError(Exception & e, AutoRef<UserData> userData) {
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
    
	client.setUrl("http://www.google.com");
	client.setConnectionTimeout(5000);
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

void s_dump() {

	AnotherHttpClient client;
    
    DumpResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    client.setUrl("http://httpbin.org/post");
    client.setRequest("POST", LinkedStringMap(), new FixedTransfer("hello", 5));
    client.execute();

	string dump = handler.getDump();
	cout << "Dump: " << dump << endl;
}

void s_query() {

	AnotherHttpClient client;
	client.setDebug(true);
    
    DumpResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    client.setUrl("http://httpbin.org/get?name=nick");
    client.setRequest("GET", LinkedStringMap(), NULL);
    client.execute();

	string dump = handler.getDump();
	cout << "Dump: " << dump << endl;
}

int main(int argc, char * args[]) {
    // s_test();
    // s_test_reuse();
	// s_post();
	// s_dump();
	s_query();

	getchar();
    
    return 0;
}