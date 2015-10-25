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
#include <libhttp-server/HttpClientThreadPool.hpp>

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

    Url url("http://www.google.com");
    MyHttpResponseHandler handler;
    
//    HttpClient client;
//    client.setFollowRedirect(true);
//    client.setHttpResponseHandler(&handler);
//    client.request(url);
    
    HttpClientThreadPool pool(5);
    pool.setFollowRedirect(true);
    pool.setHttpResponseHandler(&handler);
    pool.start();
    string method = "GET";
    pool.request(url, method, NULL, 0);
    pool.request(url, method, NULL, 0);
    pool.request(url, method, NULL, 0);
    pool.request(url, method, NULL, 0);
    pool.request(url, method, NULL, 0);
    
    getchar();
    
    pool.stop();
    
    return 0;
}
