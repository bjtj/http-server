#include "utils.hpp"
#include <libhttp-server/Url.hpp>
#include <liboslayer/Logger.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

static void test_url() {

	string text = "http://10.0.12.96:2869/upnphost/udhisapi.dll?content=uuid:27151292-513d-4dcd-912a-a2b8cdc0a128";
	
	Url url(text);

	ASSERT(url.toString(), ==, text);
	ASSERT(url.getPath(), ==, "/upnphost/udhisapi.dll");
	ASSERT(url.getPathAndQuery(), ==, "/upnphost/udhisapi.dll?content=uuid:27151292-513d-4dcd-912a-a2b8cdc0a128");
	ASSERT(url.getHost(), ==, "10.0.12.96");
	ASSERT(url.getPort(), ==, "2869");
	ASSERT(url.getScheme(), ==, "http");

	url = Url("http://localhost/");

	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "80");
	ASSERT(url.getScheme(), ==, "http");

	url = Url("https://localhost/");

	ASSERT(url.getScheme(), ==, "https");
	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "443");

	url = Url("http://username:password@localhost");

	ASSERT(url.getScheme(), ==, "http");
	ASSERT(url.getUsername(), ==, "username");
	ASSERT(url.getPassword(), ==, "password");
	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "80");
	ASSERT(url.toString(), ==, "http://username:password@localhost:80/");

	// validate
	try {
		Url::validateUrlFormat("");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.getMessage().size(), >, 0);
	}

	try {
		Url::validateUrlFormat("//");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.getMessage().size(), >, 0);
	}

	try {
		Url::validateUrlFormat("/home/user/");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.getMessage().size(), >, 0);
	}

	Url::validateUrlFormat("file:///home/user/");
}

static void test_url_tostring() {
	string u = "http://localhost:9000/page.html?a=A&b=B&c=C";
	Url url(u);
	ASSERT(url.toString(), ==, u);
}

static void test_file_url() {
	Url url("file:///test.txt");
	ASSERT(url.getPath(), ==, "/test.txt");
}

int main(int argc, char *args[]) {

	LoggerDescriptor descriptor("*");
	descriptor.setAllFormatter("basic");
	descriptor.setAllPrinter("console");
	LoggerFactory::getInstance().setLoggerDescriptor(descriptor);

	Url url;

	test_url();
	test_url_tostring();
	test_file_url();
    
    return 0;
}
