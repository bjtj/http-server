#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/FileStream.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>

using namespace std;
using namespace osl;
using namespace http;

/**
 * 
 */
class ChatHandler : public HttpRequestHandler {
private:
	vector<string> messages;
	Semaphore sem;
public:	
    ChatHandler() : sem(1) {}
    virtual ~ChatHandler() {}
	virtual void onHttpRequestContentCompleted(HttpRequest & request,
											   AutoRef<DataSink> sink,
											   HttpResponse & response) {
		if (request.isWwwFormUrlEncoded()) {
			request.parseWwwFormUrlencoded();
		}
		
		if (request.getMethod() == "POST") {
			sem.wait();
			messages.push_back(request.getParameter("msg"));
			sem.post();
		}
		
		string content;
		content.append("<ul>");
		sem.wait();
		for (vector<string>::iterator iter = messages.begin(); iter != messages.end(); iter++) {
			content.append("<li>" + *iter + "</li>");
		}
		sem.post();
		content.append("</ul>");
		content.append("<form method=\"POST\">Message: <input type=\"text\" name=\"msg\" /></form>");

		response.setStatus(200);
		response.setFixedTransfer("<html><head><title>Chat</title></head><body>" + content + "</body></html>");
	}
};


/**
 * 
 */
int main(int argc, char *args[]) {

	HttpServerConfig config;
	config["domain.host"] = "localhost";
	config["listen.port"] = "9000";
	AnotherHttpServer server(config);

	AutoRef<HttpRequestHandler> chatHandler(new ChatHandler);
	server.registerRequestHandler("/chat", chatHandler);

	server.startAsync();

	while (1) {
		FileStream fs(stdin);
		string line = fs.readline();
		if (line == "quit") {
			break;
		}
	}

	server.stop();
	cout << "[Bye]" << endl;
    
    return 0;
}
