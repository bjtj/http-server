#include <iostream>
#include <string>
#include <libhttp-server/HttpHeader.hpp>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace HTTP;
using namespace OS;
using namespace UTIL;


int main(int argc, char * args[]) {

	HttpHeader header("GET", "/hello.html?param1=value1&param2&param3=value3", "HTTP/1.1");

	HttpRequestHeader requestHeader(header);

	string method = requestHeader.getMethod();

	cout << method << endl;

	cout << requestHeader.getParameter("param1") << endl;
	cout << requestHeader.getParameter("param2") << endl;
	cout << requestHeader.getParameter("param3") << endl;

	getchar();

	return 0;
}