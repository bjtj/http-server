#include <iostream>
#include <libhttp-server/LispPage.hpp>

using namespace std;
using namespace HTTP;

#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	}

static void test_lisp_page() {
	LispPage page;
	string content;
	content = page.parseLispPage("<% (string-append *content* \"hello\")\n(string-append\n*content*\n\" world\")  %>");
	ASSERT(content, ==, "hello world");
	content = page.parseLispPage("<% (string-append\n*content*\n\"hello world\") %>");
	ASSERT(content, ==, "hello worldhello world");
}

int main(int argc, char *args[]) {

	test_lisp_page();
    
    return 0;
}
