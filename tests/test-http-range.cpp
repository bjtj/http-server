#include "utils.hpp"
#include <iostream>
#include <libhttp-server/HttpHeader.hpp>
#include <libhttp-server/HttpRequest.hpp>
#include <libhttp-server/HttpResponse.hpp>


using namespace std;
using namespace osl;
using namespace http;


static void test_http_range() {
	HttpRequest request;
	request.setHeaderField("Range", "bytes=100-0");
	ASSERT(request.containsRange(), ==, true);
	HttpRange range = request.getRange();
	range.adjustTo(200);
	ASSERT(range.toString(), ==, "100-199");
	ASSERT(range.size(), ==, 100);

	request.setHeaderField("Range", "bytes=100-120");
	range = request.getRange();
	ASSERT(range.toString(), ==, "100-120");
	ASSERT(range.size(), ==, 21);

	HttpResponse response;
	response.setRange(range, 300);
	ASSERT(response.getHeaderField("Content-Range"), ==, "bytes 100-120/300");
}


int main(int argc, char *args[]) {
	test_http_range();
    
    return 0;
}
