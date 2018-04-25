#include "utils.hpp"
#include <libhttp-server/StringDataSource.hpp>

using namespace std;
using namespace http;

static void test_string_data_source() {
	string text = "hello world";
	StringDataSource ds(text);
	char buffer[1024] = {0,};
	ASSERT(ds.eof(), ==, false);
	ASSERT(ds.read(buffer, 3), ==, 3);
	ASSERT(ds.read(buffer, sizeof(buffer)), ==, text.length() - 3);
	ASSERT(ds.eof(), ==, true);

	ds = StringDataSource(text);
	ASSERT(ds.eof(), ==, false);
	ASSERT(ds.read(buffer, sizeof(buffer)), ==, text.length());
	ASSERT(ds.eof(), ==, true);
}

int main(int argc, char *args[]) {

	test_string_data_source();
    
    return 0;
}
