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

class MyHttpResponseHandler : public HttpResponseHandler<int> {
public:
    virtual void onResponse(HttpClient<int> & httpClient, HttpHeader & responseHeader, OS::Socket & socket, int userData) {
        HttpResponseDump dump;
        string ret = dump.dump(responseHeader, socket);
        cout << userData << ret << endl;
    }
};

int main(int argc, const char * argv[]) {

    Url url("http://www.google.com");
    MyHttpResponseHandler handler;
    
//    HttpClient<int> client;
//    client.setFollowRedirect(true);
//    client.setHttpResponseHandler(&handler);
//    client.request(url, 0);
    
    HttpClientThreadPool<int> pool(5);
    pool.setFollowRedirect(true);
    pool.setHttpResponseHandler(&handler);
    pool.start();
    string method = "GET";
    int tag;
    pool.request(url, method, NULL, 0, tag);
    pool.request(url, method, NULL, 0, tag);
    pool.request(url, method, NULL, 0, tag);
    pool.request(url, method, NULL, 0, tag);
    pool.request(url, method, NULL, 0, tag);
    
    getchar();
    
    pool.stop();
    
    return 0;
}
