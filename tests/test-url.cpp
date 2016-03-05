#include <iostream>
#include <libhttp-server/Url.hpp>

using namespace std;
using namespace HTTP;

#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	}

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
}

int main(int argc, char *args[]) {

	test_url();
    
    return 0;
}
