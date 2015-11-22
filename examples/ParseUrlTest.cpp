#include <iostream>
#include <libhttp-server/Url.hpp>

using namespace std;
using namespace HTTP;

void s_print_url(Url & url) {
    cout << "URL       : " << url.toString() << endl;
    cout << "Addr      : " << url.getAddress() << endl;
    cout << "Path      : " << url.getPath() << endl;
    cout << "Query     : " << url.getQueryString() << endl;
    cout << "Path&Query: " << url.getPathAndQuery() << endl;
    cout << endl;
}

int main(int argc, char * args[]) {
    
    Url url;
    
    url = Url("http://www.google.com/?");
    s_print_url(url);
    
    url = Url("http://www.google.com");
    s_print_url(url);
    
    url = Url("http://www.google.com/");
    s_print_url(url);
    
    url = Url("http://www.google.com/path?");
    s_print_url(url);
    
    url = Url("http://www.google.com/path?param");
    s_print_url(url);
    
    url = Url("http://www.google.com/path?p1=v1&p2&p3=&p4=v4");
    s_print_url(url);
    
    url = Url("http://www.google.com/path?a=a1&a=a2&a=a3&b=&b=b1");
    s_print_url(url);
    
    return 0;
}
