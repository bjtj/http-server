#include <iostream>
#include "HttpServer.hpp"

using namespace std;
using namespace HTTP;

/**
 * @brief run server
 */
static void s_run_server() {
	
	HttpServer server(8080);

	server.start();
	
	server.stop();
}

/**
 * @brief main
 */
int main(int argc, char *args[]) {
	
	s_run_server();
	
    return 0;
}
