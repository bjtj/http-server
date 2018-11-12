#include <liboslayer/TestSuite.hpp>
#include <libhttp-server/CacheManager.hpp>

using namespace std;
using namespace osl;
using namespace http;

class CacheTestCase : public TestCase
{
public:
    CacheTestCase() : TestCase("cache-testcase") {
    }
    virtual ~CacheTestCase() {
    }
    virtual void test() {
	CacheManager cm;
	const char * text = "<html><body>hello</body></html>";
	string uid = cm.addCache("/index.html", (void*)text, strlen(text) + 1, 0);
	ASSERT(cm.getCache(uid).nil(), ==, false);
	string a((char*)cm.getCache(uid)->getData());
	string b(text);
	ASSERT(a, ==, b);
    }
};


int main(int argc, char *argv[])
{
    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new CacheTestCase));
    TestReport report(ts.testAll());
    ASSERT(report.failed(), ==, 0);   
    return 0;
}
