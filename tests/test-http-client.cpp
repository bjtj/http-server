#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/StringDataSink.hpp>
#include <liboslayer/SecureSocket.hpp>
#include "utils.hpp"

using namespace std;
using namespace osl;
using namespace http;


static string packetVisible(const string pack) {
	return Text::replaceAll(Text::replaceAll(pack, "\n", "\\n"), "\r", "\\r");
}

/**
 * 
 */
class MySocketMaker : public SocketMaker {
public:
	MySocketMaker() {}
	virtual ~MySocketMaker() {}
	virtual AutoRef<Socket> make(const string & protocol, const InetAddress & addr) {

		if (protocol == "https") {
#if defined(USE_OPENSSL)
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
#else
			throw Exception("openssl not supported");
#endif
		}
		return AutoRef<Socket>(new Socket(addr));
	}
};


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

		if (request.getPath() == "/delay") {
			unsigned long tick = tick_milli();
			unsigned long delay = (unsigned long)Text::toLong(request.getParameter("timeout"));
			idle(delay);
			response.setStatus(200);
			response.setContentType("text/plain");
			response.setFixedTransfer(("Duration - " + Text::toString(tick_milli() - tick) + " ms."));
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

		response.setFixedTransfer(ret);
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
			throw Exception("sink is nil");
		}
    }
    virtual void onError(Exception & e, AutoRef<UserData> userData) {
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

	client.close();
	
	return handler;
}

#if defined(USE_OPENSSL)
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

	client.close();
	
	return handler;
}
#endif

static void test_http_client(int port) {

	printf(" === test http client === \n");
	
	LinkedStringMap ext_headers;

	string portStr = Text::toString(port);
	
	DumpResponseHandler handler = httpRequest("http://127.0.0.1:" + portStr, "GET", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: GET\npath: /\n");

	handler = httpRequest("http://127.0.0.1:" + portStr, "SUBSCRIBE", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: SUBSCRIBE\npath: /\n");

	handler = httpRequest("http://127.0.0.1:" + portStr, "UNSUBSCRIBE", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n");

	handler = httpRequest("http://127.0.0.1:" + portStr + "/?a=A&b=B", "UNSUBSCRIBE", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n - a : A\n - b : B\n");

	handler = httpRequest("http://127.0.0.1:" + portStr + "/?b=B&a=A", "UNSUBSCRIBE", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	ASSERT(handler.getDump(), ==, "method: UNSUBSCRIBE\npath: /\n - a : A\n - b : B\n");
}

static void test_recv_timeout(int port) {

	printf(" === test recv timeout === \n");

	LinkedStringMap ext_headers;

	string portStr = Text::toString(port);
	
	DumpResponseHandler handler = httpRequest("http://127.0.0.1:" + portStr + "/delay?timeout=500", "GET", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	cout << handler.getDump() << endl;

	string err;
	try {
		handler = httpRequest("http://127.0.0.1:" + portStr + "/delay?timeout=2000", "GET", ext_headers);
	} catch (Exception & e) {
		err = e.what();
		cerr << e.what() << endl;
	}
	ASSERT(err, >, "recv timeout");

	err = "";
	try {
		handler = httpRequest("http://127.0.0.1:" + portStr + "/delay?timeout=3000", "GET", ext_headers);
	} catch (Exception & e) {
		err = e.what();
		cerr << e.what() << endl;
	}
	ASSERT(err, >, "recv timeout");
}

static void test_https_client(int port) {
#if defined(USE_OPENSSL)
	printf(" === test https client === \n");
	LinkedStringMap ext_headers;

	string portStr = Text::toString(port);
	
	DumpResponseHandler handler = httpsRequest("https://google.com", "GET", ext_headers);
	ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
	cout << handler.getDump() << endl;
#endif
}

int main(int argc, char *args[]) {

	System::getInstance()->ignoreSigpipe();

	HttpServerConfig config;
	config["listen.port"] = "0";
	config["thread.count"] = "5";
	AnotherHttpServer server(config);
	server.registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new RequestEchoHandler));
	server.startAsync();

	InetAddress addr = server.getServerAddress();
	test_http_client(addr.getPort());
	idle(10); // TODO: check ; previous connection resist?
	test_recv_timeout(addr.getPort());
	test_https_client(addr.getPort());

	server.stop();
	
    return 0;
}
