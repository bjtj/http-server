#include "utils.hpp"
#include <iostream>
#include <libhttp-server/HttpHeader.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

static void test_http_request_header() {

	string path = "/hello;session=123?a=b&b=c";
	
	HttpRequestHeader header;
	header.setPath(path);

	ASSERT(header.getParameter("session"), !=, "wow");
	ASSERT(header.getParameter("a"), ==, "b");
	ASSERT(header.getParameter("b"), ==, "c");
}

static void test_http_header_to_string() {
	HttpHeader header;
	header.setPart1("GET");
	header.setPart2("/");
	header.setPart3("HTTP/1.1");
	ASSERT(header.toString(), ==, "GET / HTTP/1.1\r\n\r\n");
	header.setHeaderField("HOST", "239.255.255.250");
	ASSERT(header.toString(), ==, "GET / HTTP/1.1\r\nHOST: 239.255.255.250\r\n\r\n");
	header.setHeaderField("SERVER", "TestServer/1.0");
	ASSERT(header.toString(), ==, "GET / HTTP/1.1\r\nHOST: 239.255.255.250\r\nSERVER: TestServer/1.0\r\n\r\n");
	header.setHeaderField("Ext", "");
	ASSERT(header.toString(), ==, "GET / HTTP/1.1\r\nHOST: 239.255.255.250\r\nSERVER: TestServer/1.0\r\nExt: \r\n\r\n");
}

int main(int argc, char *args[]) {

	test_http_request_header();
	test_http_header_to_string();
    
    return 0;
}
