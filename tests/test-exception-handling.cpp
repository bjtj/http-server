#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/StringDataSink.hpp>
#include <libhttp-server/WebServerUtil.hpp>
#include <libhttp-server/HttpException.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

/**
 * 
 */
class MyHttpRequestHandler : public HttpRequestHandler, public WebServerUtil {
private:
public:
	virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {
		if (request.getPath() == "/error") {
			throw Exception("error!");
		}
		response.setStatus(200);
		response.setContentType("text/plain");
		setFixedTransfer(response, "hello");
	}
	virtual bool onException(HttpRequest & request, HttpResponse & response, Exception & e) {
		cerr << "onException()" << endl;
		response.setStatus(500);
		response.setContentType("text/plain");
		setFixedTransfer(response, "error");
		return true;
	}
};

/**
 * 
 */
class DumpResponseHandler : public OnHttpResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
	File file;
public:
    DumpResponseHandler() {}
    virtual ~DumpResponseHandler() {}
    virtual void onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
		responseHeader = response.header();
        if (!sink.nil()) {
			try {
				dump = ((StringDataSink*)&sink)->data();
			} catch (Exception & e) {
				cout << "transfer->getString()" << endl;
			}
        }
    }
    virtual void onError(OS::Exception & e, AutoRef<UserData> userData) {
        cout << " * [Error] " << e.getMessage() << endl;
    }
	HttpResponseHeader & getResponseHeader() {
		return responseHeader;
	}
	string & getDump() {
		return dump;
	}
};

static void httpGet(const string & url, const LinkedStringMap & fields, OnHttpResponseListener * handler) {
	AnotherHttpClient client;
	client.setDebug(true);
	client.setOnHttpResponseListener(handler);
	client.setFollowRedirect(true);
	client.setUrl(url);
	client.setRequest("GET", fields);
	client.execute();
	client.closeConnection();
}

/**
 * 
 */
class ExceptionHandlingTestCase : public TestCase {
private:
	AnotherHttpServer * server;
public:
	ExceptionHandlingTestCase() : TestCase("exception handling test") {}
	virtual ~ExceptionHandlingTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		HttpServerConfig config;
		config["listen.port"] = "9000";
		config["thread.count"] = "1";
		server = new AnotherHttpServer(config);
		server->registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new MyHttpRequestHandler));
		server->startAsync();
		idle(100);
	}
	virtual void tearDown() {
		server->stop();
		delete server;
	}
	virtual void test() {
		LinkedStringMap fields;
		fields["Connection"] = "close";
		DumpResponseHandler handler;
		httpGet("http://127.0.0.1:9000/", fields, &handler);
		ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
		ASSERT(handler.getDump(), ==, "hello");
		httpGet("http://127.0.0.1:9000/error", fields, &handler);
		ASSERT(handler.getResponseHeader().getStatusCode(), ==, 500);
		ASSERT(handler.getDump(), ==, "error");
	}
};

/**
 * 
 */
class HttpExceptionTestCase : public TestCase
{
public:
    HttpExceptionTestCase() : TestCase("http-exception-test-case") {}
    virtual ~HttpExceptionTestCase() {}
	virtual void test() {
		try {
			throw HttpException(404, "Not found");
		} catch (HttpException & e) {
			ASSERT(e.statusCode(), ==, 404);
			ASSERT(e.statusString(), ==, "Not found");
			ASSERT(e.getHttpResponseHeader().toString(), ==, "HTTP/1.1 404 Not found\r\n\r\n");
		}
	}
};


/**
 * 
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ExceptionHandlingTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HttpExceptionTestCase));
	TestReport report(ts.testAll());
	report.validate();
    return 0;
}
