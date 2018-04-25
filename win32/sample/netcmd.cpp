#include <iostream>
#include <libhttp-server/AnotherHttpClient.hpp>
#include <libhttp-server/ChunkedTransfer.hpp>
#include <libhttp-server/FixedTransfer.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

class DumpResponseHandler : public OnResponseListener {
private:
	string dump;
public:
    DumpResponseHandler() {
    }
    virtual ~DumpResponseHandler() {
    }
    virtual void onTransferDone(HttpResponse & response, DataTransfer * transfer, AutoRef<UserData> userData) {
        if (transfer) {
            dump = transfer->getString();
        }
    }
    virtual void onError(Exception & e, AutoRef<UserData> userData) {
        cout << "Error/e: " << e.getMessage() << endl;
    }
	string & getDump() {
		return dump;
	}
};

void s_dump(const char * url, const char * cmd) {

	AnotherHttpClient client;
    
    DumpResponseHandler handler;
    client.setOnResponseListener(&handler);
    
    client.setFollowRedirect(true);
    client.setUrl(url);
    client.setRequest("POST", LinkedStringMap(), new FixedTransfer(cmd, strlen(cmd)));
    client.execute();

	string dump = handler.getDump();
	cout << "RESP: " << dump << endl;
}


int main(int argc, char * args[]) {

	char buffer[1024] = {0,};
	char url[4096] = {0,};

	while (fgets(buffer, sizeof(buffer), stdin)) {
		buffer[strlen(buffer) - 1] = 0;

		if (!strncmp(buffer, "url ", 4)) {
			snprintf(url, sizeof(url), "%s", buffer);
		} else {
			s_dump(url, buffer);
		}
	}
    
    return 0;
}
