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

static void httpGet(const string & url, OnHttpResponseListener * handler);
static void httpGet(const string & url, const LinkedStringMap & fields, OnHttpResponseListener * handler);


/**
 * my http request handler
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
			response.setStatus(200);
			response.setContentType("text/plain");
			response.setFixedTransfer("Hello World");
			return;
		}

		if (path == "/medium") {
			response.setStatus(200);
			response.setContentType("text/plain");
			response.setFixedTransfer(medium);
			return;
		}

		if (path == "/taketime") {
			response.setStatus(200);
			response.setContentType("text/plain");
			idle(1000);
			response.setFixedTransfer("taketime!");
			return;
		}

		if (!file.exists() || !file.isFile()) {
			response.setStatus(404);
			response.setContentType("text/plain");
			response.setFixedTransfer("Not Found");
			return;
		}

		string ext = file.extension();
		if (mimeTypes.find(ext) != mimeTypes.end()) {
			response.setStatus(200);
			response.setContentType(mimeTypes[ext]);
			response.setFileTransfer(file);
			return;
		}

		response.setStatus(200);
		response.setContentType("text/plain");
		response.setFileTransfer(file);
	}
	virtual void onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response) {
		cout << "done" << endl;
		s_last_msg = "done";
	}
};

/**
 * dump response handelr
 */
class DumpResponseHandler : public OnHttpResponseListener {
private:
	HttpResponseHeader responseHeader;
	string dump;
	File file;
public:
    DumpResponseHandler() {/**/}
    virtual ~DumpResponseHandler() {/**/}
	virtual AutoRef<DataSink> getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}
    virtual void onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
		responseHeader = response.header();
        if (!sink.nil()) {
			try {
				dump = ((StringDataSink*)&sink)->data();
			} catch (Exception & e) {
				cerr << " - [Error] sink->data()" << endl;
			}
        }
    }
    virtual void onError(OS::Exception & e, AutoRef<UserData> userData) {
        cout << " - [Error] " << e.toString() << endl;
    }
	HttpResponseHeader & getResponseHeader() {
		return responseHeader;
	}
	string & getDump() {
		return dump;
	}
};

/**
 * http server test case
 */
class HttpServerTestCase : public TestCase {
private:
	AnotherHttpServer * server;
public:
    HttpServerTestCase() : TestCase("HttpServerTestCase") {/**/}
	HttpServerTestCase(const string & name) : TestCase(name) {/**/}
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
		httpGet("http://127.0.0.1:9000/", &handler);
		ASSERT(handler.getResponseHeader().getStatusCode(), ==, 200);
		ASSERT(handler.getDump(), ==, "Hello World");
	}
};

/**
 * synchronized http client task
 */
class SynchronizedHttpClientTask : public Task {
private:
	static int idx;
	CountDownLatch & latch;
	CountDownLatch & doneLatch;
	string url;
	int id;
public:
	SynchronizedHttpClientTask(CountDownLatch & latch, CountDownLatch & doneLatch, const string & url) : latch(latch), doneLatch(doneLatch), url(url) {
		id = idx++;
	}
	virtual ~SynchronizedHttpClientTask() {}
	virtual void onTask() {
		latch.await();
		DumpResponseHandler handler;
		cout << "[" << id << "] http get" << endl;
		httpGet(url, &handler);
		cout << "[" << id << "] " << handler.getResponseHeader().getStatusCode() << endl;
		doneLatch.countDown();
	}
};

int SynchronizedHttpClientTask::idx = 0;

/**
 *
 */
class HttpServerMaxConnectionTestCase : public HttpServerTestCase {
private:
	
public:
	HttpServerMaxConnectionTestCase() : HttpServerTestCase("HttpServerMaxConnectionTestCase") {/**/}
	virtual ~HttpServerMaxConnectionTestCase() {/**/}
	virtual void test() {
		TaskThreadPool pool(50);
		pool.start();

		CountDownLatch latch(1);
		CountDownLatch doneLatch(50);
		
		for (size_t i = 0; i < 50; i++) {
			pool.setTask(AutoRef<Task>(new SynchronizedHttpClientTask(
										   latch, doneLatch, "http://127.0.0.1:9000/medium")));
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
 * @brief 
 */
class HttpServerBlockingTestCase : public HttpServerTestCase {
private:
	
public:
	HttpServerBlockingTestCase() : HttpServerTestCase("HttpServerMaxConnectionTestCase") {/**/}
	virtual ~HttpServerBlockingTestCase() {/**/}
	virtual void test() {
		size_t cnt = 3;
		TaskThreadPool pool(cnt);
		pool.start();

		CountDownLatch latch(1);
		CountDownLatch doneLatch(cnt);
		
		for (size_t i = 0; i < cnt; i++) {
			pool.setTask(AutoRef<Task>(new SynchronizedHttpClientTask(latch, doneLatch, "http://127.0.0.1:9000/taketime")));
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
 * http get
 */
static void httpGet(const string & url, OnHttpResponseListener * handler) {
	httpGet(url, LinkedStringMap(), handler);
}

/**
 * http get
 */
static void httpGet(const string & url, const LinkedStringMap & fields, OnHttpResponseListener * handler) {
	AnotherHttpClient client;
	client.setDebug(true);
    
	client.setOnHttpResponseListener(handler);
    
	client.setFollowRedirect(true);
	client.setUrl(Url(url));
	client.setRequest("GET", fields);
	client.execute();
}

/**
 *
 */
int main(int argc, char *args[]) {

	LoggerFactory::inst().setProfile("*", "basic", "console");
	
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new HttpServerTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HttpServerMaxConnectionTestCase));
	ts.addTestCase(AutoRef<TestCase>(new HttpServerBlockingTestCase));
	TestReport report(ts.testAll());
    ASSERT(report.failed(), ==, 0);
    return 0;
}
