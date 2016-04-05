#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/StringDataSink.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static string packetVisible(const string pack) {
	return Text::replaceAll(Text::replaceAll(pack, "\n", "\\n"), "\r", "\\r");
}

class RequestEchoHandler : public HttpRequestHandler {
private:
public:
    RequestEchoHandler() {}
    virtual ~RequestEchoHandler() {}

	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

		cout << "** request : " << request.getPath() << endl;
		
		string ret;
		response.setStatusCode(200);
		response.setContentType("text/plain");
		ret.append("method: " + request.getMethod() + "\n");
		ret.append("path: " + request.getPath() + "\n");
		vector<string> names = request.getParameterNames();
		for (vector<string>::iterator iter = names.begin(); iter != names.end(); iter++) {
			ret.append(" - " + *iter + " : " + request.getParameter(*iter) + "\n");
		}
		
		AutoRef<DataTransfer> transfer = request.getTransfer();
		if (!transfer.nil()) {
			string data = ((StringDataSink*)&transfer->sink())->data();
			ret.append(data);
		}

		cout << "** response / content : " << packetVisible(ret) << endl;

		setFixedTransfer(response, ret);
	}
};


class DumpResponseHandler : public OnResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
public:
    DumpResponseHandler() {}
    virtual ~DumpResponseHandler() {}
	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}
	virtual void onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
		responseHeader = response.getHeader();
        if (!sink.nil()) {
			dump = ((StringDataSink*)&sink)->data();
        } else {
			throw Exception("what the f!!!!");
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

static DumpResponseHandler httpRequest(const Url & url, const string & method, const LinkedStringMap & headers) {

	cout << " ** url : " << url.toString() << endl;
	cout << " ** method : " << method << endl;
	
	DumpResponseHandler handler;
	AnotherHttpClient client;
	client.setDebug(true);

	client.setConnectionTimeout(5000);
	client.setRecvTimeout(5000);
    
	client.setOnResponseListener(&handler);
    
	client.setFollowRedirect(true);
	client.setUrl(url);
	client.setRequest(method, headers);
	client.execute();
	
	return handler;
}


static void test_http_client() {
	DumpResponseHandler handler = httpRequest("http://127.0.0.1:9999", "GET", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: GET\npath: /\n");

	handler = httpRequest("http://127.0.0.1:9999", "SUBSCRIBE", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: SUBSCRIBE\npath: /\n");

	handler = httpRequest("http://127.0.0.1:9999", "UNSUBSCRIBE", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n");

	handler = httpRequest("http://127.0.0.1:9999/?a=A&b=B", "UNSUBSCRIBE", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n - a : A\n - b : B\n");

	handler = httpRequest("http://127.0.0.1:9999/?b=B&a=A", "UNSUBSCRIBE", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n - a : A\n - b : B\n");
}

int main(int argc, char *args[]) {

	HttpServerConfig config;
	config["listen.port"] = "9999";
	config["thread.count"] = "5";
	AnotherHttpServer server(config);
	server.registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new RequestEchoHandler));
	server.startAsync();

	test_http_client();

	server.stop();
	
    return 0;
}
