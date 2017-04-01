#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/StringDataSink.hpp>
#include <libhttp-server/WebServerUtil.hpp>
#include <liboslayer/SecureSocket.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static string packetVisible(const string pack) {
	return Text::replaceAll(Text::replaceAll(pack, "\n", "\\n"), "\r", "\\r");
}

class MySocketMaker : public SocketMaker {
public:
	MySocketMaker() {}
	virtual ~MySocketMaker() {}
	virtual AutoRef<Socket> make(const string & protocol, const InetAddress & addr) {

		if (protocol == "https") {
			class MyVerifier : public CertificateVerifier {
			public:
				MyVerifier() {}
				virtual ~MyVerifier() {}
				virtual bool onVerify(const VerifyError & err, const Certificate & cert) {
					return true;
				}
			};
			SecureSocket * sock = new SecureSocket(addr);
			sock->setVerifier(AutoRef<CertificateVerifier>(new MyVerifier));
			return AutoRef<Socket>(sock);
		}
		return AutoRef<Socket>(new Socket(addr));
	}
};


class RequestEchoHandler : public HttpRequestHandler, public WebServerUtil {
private:
public:
    RequestEchoHandler() {}
    virtual ~RequestEchoHandler() {}

	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

		cout << "** request : " << request.getPath() << endl;

		if (request.getPath() == "/delay") {
			unsigned long tick = tick_milli();
			unsigned long delay = (unsigned long)Text::toLong(request.getParameter("timeout"));
			idle(delay);
			response.setStatus(200);
			response.setContentType("text/plain");
			setFixedTransfer(response, ("Duration - " + Text::toString(tick_milli() - tick) + " ms."));
			return;
		}
		
		string ret;
		response.setStatus(200);
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


class DumpResponseHandler : public OnHttpResponseListener {
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
		responseHeader = response.header();
        if (!sink.nil()) {
			dump = ((StringDataSink*)&sink)->data();
        } else {
			throw Exception("sink->data() failed");
		}
    }
    virtual void onError(OS::Exception & e, AutoRef<UserData> userData) {
        cout << " [Error] " << e.toString() << endl;
    }
	HttpResponseHeader & getResponseHeader() {
		return responseHeader;
	}
	string & getDump() {
		return dump;
	}
};

static DumpResponseHandler httpRequest(const Url & url, const string & method, const LinkedStringMap & headers) {

	cout << " ** Request/url : " << url.toString() << endl;
	cout << " ** Request/method : " << method << endl;
	
	DumpResponseHandler handler;
	AnotherHttpClient client;
	client.setDebug(true);

	client.setConnectionTimeout(1000);
	client.setRecvTimeout(1000);
    
	client.setOnHttpResponseListener(&handler);
    
	client.setFollowRedirect(true);
	client.setUrl(url);
	client.setRequest(method, headers);
	client.execute();
	
	return handler;
}

static DumpResponseHandler httpsRequest(const Url & url, const string & method, const LinkedStringMap & headers) {

	cout << " ** Request/url : " << url.toString() << endl;
	cout << " ** Request/method : " << method << endl;

	DumpResponseHandler handler;
	AnotherHttpClient client(AutoRef<SocketMaker>(new MySocketMaker));
	client.setDebug(true);

	client.setConnectionTimeout(1000);
	client.setRecvTimeout(1000);
    
	client.setOnHttpResponseListener(&handler);
    
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

static void test_recv_timeout() {
	DumpResponseHandler handler = httpRequest("http://127.0.0.1:9999/delay?timeout=500", "GET", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	cout << handler.getDump() << endl;

	string err;
	try {
		handler = httpRequest("http://127.0.0.1:9999/delay?timeout=2000", "GET", LinkedStringMap());
	} catch (Exception & e) {
		err = e.what();
		cerr << e.what() << endl;
	}
	ASSERT(err, >, "recv timeout");

	err = "";
	try {
		handler = httpRequest("http://127.0.0.1:9999/delay?timeout=3000", "GET", LinkedStringMap());
	} catch (Exception & e) {
		err = e.what();
		cerr << e.what() << endl;
	}
	ASSERT(err, >, "recv timeout");
}

static void test_https_client() {
	DumpResponseHandler handler = httpsRequest("https://google.com", "GET", LinkedStringMap());
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	cout << handler.getDump() << endl;
}

int main(int argc, char *args[]) {

	HttpServerConfig config;
	config["listen.port"] = "9999";
	config["thread.count"] = "5";
	AnotherHttpServer server(config);
	server.registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new RequestEchoHandler));
	server.startAsync();

	test_http_client();
	test_recv_timeout();
	test_https_client();

	server.stop();
	
    return 0;
}
