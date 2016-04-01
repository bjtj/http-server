#include "utils.hpp"
#include <libhttp-server/StringDataSink.hpp>

using namespace std;
using namespace HTTP;

static void test_string_data_sink() {
	StringDataSink sink;
	string text = "hello world";
	sink.write(text.c_str(), 5);
	ASSERT(sink.data(), ==, text.substr(0, 5));
	sink.write(text.c_str() + 5, text.length() - 5);
	ASSERT(sink.data(), ==, text);
}

int main(int argc, char *args[]) {

	test_string_data_sink();
    
    return 0;
}
