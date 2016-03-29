#include <iostream>
#include "utils.hpp"
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/AnotherHttpClient.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;

static string s_last_msg;

class MyHttpRequestHandler : public HttpRequestHandler {
private:
	HttpServerConfig config;
	map<string, string> mimeTypes;
public:
    MyHttpRequestHandler(HttpServerConfig & config) : config(config) {
		mimeTypes["mp4"] = "video/mp4";
	}
    virtual ~MyHttpRequestHandler() {}

	virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {

		string basePath = config["base.path"];
		string path = request.getPath();
		string fullpath = basePath + path;
		File file(fullpath);

		if (path == "/") {
			response.setStatusCode(200);
			response.setContentType("text/plain");
			setFixedTransfer(response, "Hello World");
			return;
		}

		if (!file.exists() || !file.isFile()) {
			response.setStatusCode(404);
			response.setContentType("text/plain");
			setFixedTransfer(response, "Not Found");
			return;
		}

		string ext = file.getExtension();
		if (mimeTypes.find(ext) != mimeTypes.end()) {
			response.setStatusCode(200);
			response.setContentType(mimeTypes[ext]);
			setFileTransfer(response, file);
			return;
		}

		response.setStatusCode(200);
		response.setContentType("text/plain");
		setFileTransfer(response, file);
	}
	virtual void onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response) {
		cout << "done" << endl;
		s_last_msg = "done";
	}
};

class DumpResponseHandler : public OnResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
	File file;
public:
    DumpResponseHandler() {}
    virtual ~DumpResponseHandler() {}
    virtual void onTransferDone(HttpResponse & response, DataTransfer * transfer, AutoRef<UserData> userData) {
		responseHeader = response.getHeader();
        if (transfer) {
			try {
				dump = transfer->getString();
			} catch (Exception e) {
				cout << "transfer->getString()" << endl;
			}
        }
    }
    virtual void onError(OS::Exception & e, AutoRef<UserData> userData) {
        cout << "Error/e: " << e.getMessage() << endl;
    }
	HttpResponseHeader & getResponseHeader() {
		return responseHeader;
	}
	string & getDump() {
		return dump;
	}
};

class MyTestSuite {
private:
	AnotherHttpServer * server;
public:
    MyTestSuite() {}
    virtual ~MyTestSuite() {}

	virtual void startUp() {
		HttpServerConfig config;
		config["listen.port"] = "9000";
		config["base.path"] = "~/test";
		server = new AnotherHttpServer(config);
		server->registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new MyHttpRequestHandler(config)));
		server->startAsync();
	}

	virtual void tearDown() {
		server->stop();
		delete server;
	}

	virtual void doTests() {
		test_get();
	}

	void test_get() {
		DumpResponseHandler handler;
		httpGet("http://localhost:9000/", LinkedStringMap(), &handler);
		ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
		ASSERT(handler.getDump(), ==, "Hello World");
	}

	void test_media() {
		
	}

	void httpGet(const string & url, const LinkedStringMap & fields, OnResponseListener * handler) {
		AnotherHttpClient client;
		client.setDebug(true);
    
		client.setOnResponseListener(handler);
    
		client.setFollowRedirect(true);
		client.setUrl(url);
		client.setRequest("GET", fields, NULL);
		client.execute();
	}
};


int main(int argc, char *args[]) {

	MyTestSuite ts;

	try {
		ts.startUp();
		ts.doTests();
		ts.tearDown();
	} catch (const char * e) {
		cout << e << endl;
	} catch (const string & e) {
		cout << e << endl;
	} catch (Exception & e) {
		cout << e.getMessage() << endl;
	}

	ASSERT(s_last_msg, ==, "done");
    
    return 0;
}
