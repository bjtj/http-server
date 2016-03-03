#include <iostream>
#include <libhttp-server/HttpHeader.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	}

static void test_http_request_header() {

	string path = "/hello;session=123?a=b&b=c";
	
	HttpRequestHeader header;
	header.setPath(path);

	ASSERT(header.getParameter("session"), !=, "wow");
	ASSERT(header.getParameter("a"), ==, "b");
	ASSERT(header.getParameter("b"), ==, "c");
}

int main(int argc, char *args[]) {

	test_http_request_header();
    
    return 0;
}
