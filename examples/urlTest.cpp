#include <iostream>
#include <libhttp-server/Url.hpp>

using namespace std;
using namespace HTTP;

int main(int argc, char *args[]) {

	Url url("http://10.0.12.96:2869/upnphost/udhisapi.dll?content=uuid:27151292-513d-4dcd-912a-a2b8cdc0a128");

	cout << url.toString() << endl;
	cout << url.getPath() << endl;
	cout << url.getPathAndQuery() << endl;
    
    return 0;
}
