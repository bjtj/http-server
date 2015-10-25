//
//  main.cpp
//  http-server-console
//
//  Created by TJ on 2015. 10. 25..
//  Copyright © 2015년 TJ. All rights reserved.
//

#include <iostream>
#include <string>
#include <libhttp-server/HttpClient.hpp>

using namespace HTTP;
using namespace std;

class MyHttpResponseHandler : public HttpResponseHandler {
public:
    virtual void onResponse(HttpClient & httpClient, HttpHeader & responseHeader, OS::Socket & socket) {
        HttpResponseDump dump;
        string ret = dump.dump(responseHeader, socket);
        cout << ret << endl;
    }
};

int main(int argc, const char * argv[]) {

    HttpClient client;
    Url url("http://www.google.com");
    MyHttpResponseHandler handler;
    client.setFollowRedirect(true);
    client.setHttpResponseHandler(&handler);
    client.request(url);
    
    return 0;
}
