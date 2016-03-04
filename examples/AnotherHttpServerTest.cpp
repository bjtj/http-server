#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/FileReaderWriter.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Logger.hpp>

#include <vector>
#include <map>

#include <libhttp-server/Packet.hpp>
#include <libhttp-server/HttpRequest.hpp>
#include <libhttp-server/HttpHeaderReader.hpp>
#include <libhttp-server/Connection.hpp>
#include <libhttp-server/Communication.hpp>
#include <libhttp-server/ConnectionManager.hpp>
#include <libhttp-server/DataTransfer.hpp>
#include <libhttp-server/AnotherHttpServer.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static const Logger & logger = LoggerFactory::getDefaultLogger();

class SampleHttpRequestHandler : public HttpRequestHandler {
private:
	string prefix;

public:
	SampleHttpRequestHandler(string prefix) : prefix(prefix) {}
	virtual ~SampleHttpRequestHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/html");
        
        responseHeader.setConnection("close");
        
        logger.logv(request.getHeader().toString());
        
        string content = prefix + "Method: " + request.getMethod() + " / Path: " + request.getPath() + "\r\n";
        content.append("<form method='post'><input type='text' name='name' /></form>");
        
        setFixedTransfer(response, content);
    }
    
    virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
        
        AutoRef<DataTransfer> transfer = request.getTransfer();
        if (!transfer.empty()) {
            logger.logv(transfer->getString());
        }
    }
};


class RfcHttpRequestHandler : public HttpRequestHandler {
private:
public:
    RfcHttpRequestHandler() {}
    virtual ~RfcHttpRequestHandler() {}
    
    virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        response.setStatusCode(200, "OK");
        response.setContentType("text/plain");
        
        responseHeader.setConnection("close");
        
        logger.logv(request.getHeader().toString());

		 setFileTransfer(response, "res/rfc3261.txt");
    }
    
    virtual void onHttpRequestContent(HttpRequest & request, HttpResponse & response, Packet & packet) {
    }
    
    virtual void onHttpRequestContentCompleted(HttpRequest & request, HttpResponse & response) {
    }
};


class PT : public Thread {
private:
	ConnectionManager & cm;
public:
	PT(ConnectionManager & cm) : cm(cm) {
	}
	virtual ~PT() {
	}
	virtual void run() {
		while (!interrupted()) {
			cm.poll(1000);
		}
	}
};


void old() {

	SimpleHttpRequestHandlerDispatcher dispatcher;

	AutoRef<HttpRequestHandler> hello(new SampleHttpRequestHandler("Hello"));
	dispatcher.registerRequestHandler("/hello", hello);

	AutoRef<HttpRequestHandler> byebye(new SampleHttpRequestHandler("Byebye!"));
	dispatcher.registerRequestHandler("/byebye", byebye);

    HttpCommunicationMaker hcm(&dispatcher);
	ConnectionManager cm(hcm, 20);

	cm.start(8083);

	PT pt(cm);
	pt.start();

	getchar();

	pt.interrupt();
	pt.join();

	cm.stop();
}

class SampleChunkedHttpClient {
private:
	string path;
	InetAddress remoteAddr;
public:
	SampleChunkedHttpClient(const char * remoteAddr, int port, const string & path) : remoteAddr(remoteAddr, port), path(path) {
	}
	virtual ~SampleChunkedHttpClient() {
	}

	void send() {

		try {

			Socket socket(remoteAddr);
			socket.connect();
			string header = "POST " + path + " HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n";
			socket.send(header.c_str(), (int)header.length());

			for (size_t i = 0; i < 4096; i++) {
				int size = 5;
				
				int append = i % 10;
				string dataString = Text::toString(i);
				dataString.append(size, 'a' + append);
				dataString.append("\n");
				dataString.append("\r\n");

				string sizeString = Text::toHexString(dataString.length() - 2) + "\r\n";
				socket.send(sizeString.c_str(), sizeString.length());
				socket.send(dataString.c_str(), dataString.length());
			}

			string last = "0\r\n\r\n";
			int sentLen = socket.send(last.c_str(), last.length());

			if (sentLen != last.length()) {
				logger.loge("expected: " + Text::toString(last.length()) + ", but: " + Text::toString(sentLen));
			}

			// wait

			while (1) {
				char buffer[1024] = {0,};
				int len = socket.recv(buffer, sizeof(buffer));
				logger.logv(string(buffer, len));
			}

		} catch (IOException e) {
			logger.loge("sample chunked client exception/" + e.getMessage());
		}
	}
};


class SampleFixedHttpClient {
private:
	string path;
	InetAddress remoteAddr;
public:
	SampleFixedHttpClient(const char * remoteAddr, int port, const string & path) : remoteAddr(remoteAddr, port), path(path) {
	}
	virtual ~SampleFixedHttpClient() {
	}

	void send() {

		try {

            File file("res/rfc3261.txt");
			FileReader reader(file);
			string content = reader.dumpAsString();

			Socket socket(remoteAddr);
			socket.connect();
			string header = "POST " + path + " HTTP/1.1\r\nContent-Length: " + Text::toString(content.length()) + "\r\n\r\n";
			socket.send(header.c_str(), (int)header.length());
			int sentLen = socket.send(content.c_str(), content.length());

			if (sentLen != content.length()) {
				logger.loge("expected: " + Text::toString(content.length()) + ", but: " + Text::toString(sentLen));
			}

			// wait

			while (1) {
				char buffer[1024] = {0,};
				int len = socket.recv(buffer, sizeof(buffer));
				logger.logv(string(buffer, len));
			}

		} catch (IOException e) {
			logger.loge("sample fixed client exception/" + e.getMessage());
		}
	}
};


class SampleGetHttpClient {
private:
	string path;
	InetAddress remoteAddr;
public:
    SampleGetHttpClient(const char * remoteAddr, int port, const string & path) : remoteAddr(remoteAddr, port), path(path) {
    }
    virtual ~SampleGetHttpClient() {
    }
    
    void send() {
        
        try {
            Socket socket(remoteAddr);
            socket.connect();
            string header = "GET " + path + " HTTP/1.1\r\nContent-Length: 0\r\n\r\n";
            socket.send(header.c_str(), (int)header.length());
            
            // wait
            
            while (1) {
                char buffer[1024] = {0,};
                int len = socket.recv(buffer, sizeof(buffer));
                logger.logv(string(buffer, len));
            }
            
        } catch (IOException e) {
            logger.loge("sample fixed client exception/" + e.getMessage());
        }
    }
};


size_t readline(char * buffer, size_t max) {
	if (fgets(buffer, (int)max - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return strlen(buffer);
	}
	return 0;
}

void recent() {

	bool done = false;
	AnotherHttpServer server(8083);

	AutoRef<HttpRequestHandler> hello(new SampleHttpRequestHandler("Hello"));
	server.registerRequestHandler("/hello", hello);

	AutoRef<HttpRequestHandler> byebye(new SampleHttpRequestHandler("Byebye!"));
	server.registerRequestHandler("/byebye", byebye);
    
	AutoRef<HttpRequestHandler> rfc(new RfcHttpRequestHandler);
    server.registerRequestHandler("/rfc", rfc);

	server.startAsync();

	while (!done) {
		char buffer[1024] = {0,};
		readline(buffer, sizeof(buffer));
		if (!strcmp(buffer, "q")) {
			break;
		}

		if (!strcmp(buffer, "c")) {
			SampleChunkedHttpClient client("127.0.0.1", 8083, "/hello");
			client.send();
		}

		if (!strcmp(buffer, "f")) {
			SampleFixedHttpClient client("127.0.0.1", 8083, "/hello");
			client.send();
		}
        
        if (!strcmp(buffer, "g")) {
            SampleGetHttpClient client("127.0.0.1", 8083, "/hello");
            client.send();
        }
        
	}

	server.stop();
}

int main(int argc, char * args[]) {

	recent();

	return 0;
}
