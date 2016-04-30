#include <liboslayer/Logger.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/TaskThreadPool.hpp>
#include <liboslayer/CountDownLatch.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/StringDataSink.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;


static string s_last_msg;

static void httpGet(const string & url, const LinkedStringMap & fields, OnResponseListener * handler);


/**
 *
 */
class MyHttpRequestHandler : public HttpRequestHandler {
private:
	HttpServerConfig config;
	map<string, string> mimeTypes;
	string medium;;
public:
    MyHttpRequestHandler(HttpServerConfig & config) : config(config), medium(1024 * 1024, 'a') {
		mimeTypes["mp4"] = "video/mp4";
	}
    virtual ~MyHttpRequestHandler() {}

	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	virtual void onHttpRequestContentCompleted(HttpRequest & request, AutoRef<DataSink> sink, HttpResponse & response) {

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

		if (path == "/medium") {
			response.setStatusCode(200);
			response.setContentType("text/plain");
			setFixedTransfer(response, medium);
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

/**
 *
 */
class DumpResponseHandler : public OnResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
	File file;
public:
    DumpResponseHandler() {}
    virtual ~DumpResponseHandler() {}
	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}
    virtual void onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
		responseHeader = response.getHeader();
        if (!sink.nil()) {
			try {
				dump = ((StringDataSink*)&sink)->data();
			} catch (Exception & e) {
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

/**
 *
 */
class HttpServerTestCase : public TestCase {
private:
	AnotherHttpServer * server;
public:
    HttpServerTestCase() : TestCase("HttpServerTestCase") {}
	HttpServerTestCase(const string & name) : TestCase(name) {}
    virtual ~HttpServerTestCase() {}

	virtual void setUp(TestEnvironment & env) {
		HttpServerConfig config;
		config["listen.port"] = "9000";
		config["base.path"] = "~/test";
		config["thread.count"] = "10";
		server = new AnotherHttpServer(config);
		server->registerRequestHandler("/*", AutoRef<HttpRequestHandler>(new MyHttpRequestHandler(config)));
		server->startAsync();
		idle(100);
	}

	virtual void tearDown() {
		server->stop();
		delete server;
	}

	virtual void test() {
		DumpResponseHandler handler;
		httpGet("http://localhost:9000/", LinkedStringMap(), &handler);
		ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
		ASSERT(handler.getDump(), ==, "Hello World");
	}
};

/**
 *
 */
class SynchrosizedHttpClientTask : public Task {
private:
	static int idx;
	CountDownLatch & latch;
	CountDownLatch & doneLatch;
	string url;
	int id;
public:
	SynchrosizedHttpClientTask(CountDownLatch & latch, CountDownLatch & doneLatch, const string & url) : latch(latch), doneLatch(doneLatch), url(url) {
		id = idx++;
	}
	virtual ~SynchrosizedHttpClientTask() {}
	virtual void doTask() {

		latch.await();
		
		DumpResponseHandler handler;
		httpGet(url, LinkedStringMap(), &handler);
		// ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
		// ASSERT(handler.getDump(), ==, "Hello World");
		cout << "[" << id << "] " << handler.getResponseHeader().getStatusCode() << endl;

		doneLatch.countDown();
	}
};

int SynchrosizedHttpClientTask::idx = 0;

/**
 *
 */
class HttpServerMaxConnectionTestCase : public HttpServerTestCase {
private:
	
public:
	HttpServerMaxConnectionTestCase() : HttpServerTestCase("HttpServerMaxConnectionTestCase") {}
	virtual ~HttpServerMaxConnectionTestCase() {}
	virtual void test() {
		TaskThreadPool pool(50);
		pool.start();

		CountDownLatch latch(1);
		CountDownLatch doneLatch(50);
		
		for (size_t i = 0; i < 50; i++) {
			pool.setTask(AutoRef<Task>(new SynchrosizedHttpClientTask(latch, doneLatch, "http://localhost:9000/medium")));
		}
		
		idle(100);
		latch.countDown();

		unsigned long tick = tick_milli();
		doneLatch.await();

		cout << " ** stop / dur : " << (tick_milli() - tick) << " ms." << endl;

		pool.stop();
	}
};

/**
 *
 */
int main(int argc, char *args[]) {

	LoggerFactory::getInstance().setLoggerDescriptorSimple("*", "basic", "console");
	
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new HttpServerTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HttpServerMaxConnectionTestCase));
	TestReport report(ts.testAll());
    ASSERT(report.failed(), ==, 0);
    return 0;
}

/**
 *
 */
static void httpGet(const string & url, const LinkedStringMap & fields, OnResponseListener * handler) {
	AnotherHttpClient client;
	client.setDebug(true);
    
	client.setOnResponseListener(handler);
    
	client.setFollowRedirect(true);
	client.setUrl(url);
	client.setRequest("GET", fields);
	client.execute();
}
