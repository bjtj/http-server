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
	
	content = page.parseLispPage("<% (setq *content* \"\") (setq *content* (string-append *content* \"hello\"))\n(setq *content* (string-append\n*content*\n\" world\"))  %>");
	ASSERT(content, ==, "hello world");
	content = page.parseLispPage("<% (setq *content* (string-append\n*content*\n\"hello world\")) %>");
	ASSERT(content, ==, "hello worldhello world");
}

static void test_apply_properties() {
	LispPage page;
	map<string, string> props;
	props["base.path"] = "/home/user/steve/";
	props["date_and_path.of.the.record"] = "2016-03-05 /home/user/";
	page.applyProperties(props);
	ASSERT(page.env().scope().get("*base-path*")->toString(), ==, "/home/user/steve/");
	ASSERT(page.env().scope().get("*date-and-path-of-the-record*")->toString(), ==, "2016-03-05 /home/user/");
}

int main(int argc, char *args[]) {

	test_lisp_page();
	test_apply_properties();
    
    return 0;
}
