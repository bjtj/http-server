#include <iostream>
#include <libhttp-server/BufferIndicator.hpp>
#include "utils.hpp"

using namespace std;
using namespace HTTP;

static void test_buffer_indicator() {
	
	BufferIndicator ind(1024);
	
	ASSERT(ind.position(), ==, 0);
	ASSERT(ind.size(), ==, 1024);
	ind.position(100);
	ASSERT(ind.position(), ==, 100);
	ASSERT(ind.remain(), ==, true);
	ASSERT(ind.remaining(), ==, 1024 - 100);
	ASSERT(ind.size(), ==, 1024);
	ASSERT(ind.adjustReadSize(1024), ==, 1024 - 100);
	ind.offset(ind.adjustReadSize(1024));
	ASSERT(ind.remain(), ==, false);
	ASSERT(ind.remaining(), ==, 0);
}

int main(int argc, char *args[]) {

	test_buffer_indicator();
    
    return 0;
}
