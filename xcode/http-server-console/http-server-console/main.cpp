//
//  main.cpp
//  http-server-console
//
//  Created by TJ on 2015. 10. 25..
//  Copyright © 2015년 TJ. All rights reserved.
//

#include <iostream>
#include <string>
#include <liboslayer/StringElement.hpp>
#include <libhttp-server/HttpClient.hpp>
#include <libhttp-server/HttpClientThreadPool.hpp>

using namespace HTTP;
using namespace std;
using namespace UTIL;

class MyHttpResponseHandler : public HttpResponseHandler<int> {
public:
    virtual void onHttpResponse(HttpClient<int> & httpClient, const HttpHeader & responseHeader, const string & content, int userData) {

        cout << userData << content << endl;
    }
    virtual void onError(HttpClient<int> & httpClient, int userData) {
        
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
    pool.setHttpClientPollListener(&handler);
//    pool.setHttpResponseHandler(&handler);
    pool.start();
    string method = "GET";
    int tag = 0;
    pool.request(url, method, StringMap(), NULL, 0, tag);
    pool.request(url, method, StringMap(), NULL, 0, tag);
    pool.request(url, method, StringMap(), NULL, 0, tag);
    pool.request(url, method, StringMap(), NULL, 0, tag);
    pool.request(url, method, StringMap(), NULL, 0, tag);
    
    getchar();
    
    pool.stop();
    
    return 0;
}
