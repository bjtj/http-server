#include <liboslayer/TestSuite.hpp>
#include <libhttp-server/Cookie.hpp>
#include <libhttp-server/HttpRequest.hpp>
#include <libhttp-server/HttpResponse.hpp>
#include <vector>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

class CookieTestCase : public TestCase {
public:
    CookieTestCase() : TestCase("cookie-test-case") {
	}
    virtual ~CookieTestCase() {
	}
	virtual void test() {
		{
			Cookie cookie;
			cookie["a"] = "A";
			cookie["b"] = "B";
			ASSERT(cookie.toString(), ==, "a=A; b=B");
			cookie["c"] = "";
			ASSERT(cookie.toString(), ==, "a=A; b=B; c");
		}

		{
			Cookie cookie("reg_fb_gate=deleted; Expires=Thu, 01 Jan 1970 00:00:01 GMT; Path=/; Domain=.example.com; HttpOnly");
			ASSERT(cookie["reg_fb_gate"], ==, "deleted");
			ASSERT(cookie["Expires"], ==, "Thu, 01 Jan 1970 00:00:01 GMT");
			ASSERT(cookie["Path"], ==, "/");
			ASSERT(cookie["Domain"], ==, ".example.com");
			ASSERT(cookie.contains("HttpOnly"), ==, true);
			ASSERT(cookie["HttpOnly"], ==, "");
		}

		{
			Cookie cookie("  reg_fb_gate = deleted ; Expires = Thu, 01 Jan 1970 00:00:01 GMT ; Path = / ; Domain = .example.com ; HttpOnly=  ");
			ASSERT(cookie["reg_fb_gate"], ==, "deleted");
			ASSERT(cookie["Expires"], ==, "Thu, 01 Jan 1970 00:00:01 GMT");
			ASSERT(cookie["Path"], ==, "/");
			ASSERT(cookie["Domain"], ==, ".example.com");
			ASSERT(cookie.contains("HttpOnly"), ==, true);
			ASSERT(cookie["HttpOnly"], ==, "");
		}

		{
			LinkedStringMap m;
			m["made_write_conn"] = "1295214458";
			m["Path"] = "/";
			m["Domain"] = ".example.com";
			Cookie cookie(m);
			ASSERT(cookie.toString(), ==, "made_write_conn=1295214458; Path=/; Domain=.example.com")
		}

		{
			HttpRequest request;
			request.appendHeaderField("Cookie", "a=A; b=B");
			request.appendHeaderField("Cookie", "c=C; d=D");
			vector<Cookie> cookies = request.getCookies();
			ASSERT(cookies.size(), ==, 2);
			ASSERT(cookies[0].toString(), ==, "a=A; b=B");
			ASSERT(cookies[0]["a"], ==, "A");
			ASSERT(cookies[0]["b"], ==, "B");
			ASSERT(cookies[1].toString(), ==, "c=C; d=D");
			ASSERT(cookies[1]["c"], ==, "C");
			ASSERT(cookies[1]["d"], ==, "D");

			HttpResponse response;
			response.setStatus(200);
			response.setCookies(cookies);
			ASSERT(response.toString(), ==, "HTTP/1.1 200 OK\r\nSet-Cookie: a=A; b=B\r\nSet-Cookie: c=C; d=D\r\n\r\n");
		}
	}
};


int main(int argc, char *argv[])
{
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new CookieTestCase));
	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
