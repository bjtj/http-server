#include <liboslayer/Text.hpp>
#include "HttpClient.hpp"
#include "ChunkedReader.hpp"
#include "Logger.hpp"

using namespace std;;
using namespace OS;
using namespace UTIL;

namespace HTTP {

	static Logger & logger = Logger::getLogger();
    
    
    HttpResponseDump::HttpResponseDump() {
        
    }
    
    HttpResponseDump::~HttpResponseDump() {
        
    }
    
    string HttpResponseDump::dump(HttpHeader & responseHeader, Socket & socket) {
        
        string ret;
        
        if (responseHeader.isChunkedTransfer()) {
            ChunkedReader reader(socket);
            int size = 0;
            while ((size = reader.readChunkSize()) > 0) {
                char * buffer = new char[size];
                int len = reader.readChunkData(size, buffer, size);
                string chunk(buffer, len);
                delete buffer;
                
                ret.append(chunk);
            }
        } else {
            int contentLength = responseHeader.getContentLength();
            char buffer[1024] = {0,};
            int len = 0;
            int total = 0;
            
            while ((len = socket.recv(buffer, sizeof(buffer))) > 0) {
                
                string chunk(buffer, len);
                
                ret.append(chunk);
                
                total += len;
                if (total >= contentLength) {
                    break;
                }
            }
        }
        
        return ret;
    }
    
	
	HttpClient::HttpClient() : responseHandler(NULL), socket(NULL), followRedirect(false) {
		httpProtocol = "HTTP/1.1";
		defaultHeaderFields["User-Agent"] = "Cross-Platform/0.1 HTTP/1.1 HttpClient/0.1";
	}
	
	HttpClient::~HttpClient() {
	}

	void HttpClient::setFollowRedirect(bool followRedirect) {
		this->followRedirect = followRedirect;
	}

	void HttpClient::setHttpResponseHandler(HttpResponseHandler * responseHandler) {
		this->responseHandler = responseHandler;
	}

	void HttpClient::request(Url & url) {
		map<string, string> empty;
		request(url, "GET", empty, NULL, 0);
	}

	void HttpClient::request(Url & url, string method, char * data, int len) {
		map<string, string> empty;
		request(url, method, empty, data, len);
	}

	void HttpClient::request(Url & url,
							 string method,
							 map<string, string> & additionalHeaderFields,
							 char * data,
							 int len) {

		if (!socket) {

			HttpHeader requestHeader =
				makeRequestHeader(method, url.getPath(), httpProtocol, url.getAddress());
			requestHeader.appendHeaderFields(additionalHeaderFields);
			
			socket = connect(url);
			sendRequestPacket(*socket, requestHeader, data, len);
		
			HttpHeader responseHeader = readResponseHeader(*socket);
			if (followRedirect && checkIf302(responseHeader)) {
				responseHeader = processRedirect(*socket, requestHeader, responseHeader, data, len);
			}
			
			if (responseHandler) {
				responseHandler->onResponse(*this, responseHeader, *socket);
			}
			disconnect(socket);
			socket = NULL;
		}
	}

	Socket * HttpClient::connect(Url & url) {
		Socket * socket = new Socket(url.getHost().c_str(), url.getIntegerPort());
		socket->connect();
		return socket;
	}

	void HttpClient::disconnect(Socket * socket) {
		socket->close();
		delete socket;
	}

	HttpHeader HttpClient::makeRequestHeader(string method, string path, string protocol, string targetHost) {
		HttpHeader header;
		header.setPart1(method);
		header.setPart2(path);
		header.setPart3(protocol);
		if (!targetHost.empty()) {
			header.setHeaderField("Host", targetHost);
		}
		header.appendHeaderFields(defaultHeaderFields);
		return header;
	}

	void HttpClient::sendRequestPacket(Socket & socket, HttpHeader & header, char * buffer, int len) {
		header.setContentLength(len);
		string headerStr = header.toString();
		logger.logd(headerStr);
		socket.send(headerStr.c_str(), headerStr.length());
		if (buffer && len > 0) {
			socket.send(buffer, len);
		}
	}

	HttpHeader HttpClient::readResponseHeader(Socket & socket) {
		HttpHeaderReader headerReader;
		char buffer;
		while (!headerReader.complete() && socket.recv(&buffer, 1) > 0) {
			headerReader.read(&buffer, 1);
		}
		if (!headerReader.complete()) {
			throw -1;
		}
		logger.logd(headerReader.getHeader().toString());
		return headerReader.getHeader();
	}

	bool HttpClient::checkIf302(HttpHeader & responseHeader) {
		int statusCode = Text::toInt(responseHeader.getPart2());
		return (statusCode == 302);
	}

	int HttpClient::consume(OS::Socket & socket, int length) {
		char buffer[1024] = {0,};
		int len;
		int total = 0;
		while ((len = socket.recv(buffer, sizeof(buffer))) > 0) {
			string str(buffer, len);
			logger.logd(str);
			total += len;
			if (total >= length) {
				break;
			}
		}
		return total;
	}

	HttpHeader HttpClient::processRedirect(OS::Socket & socket,
										   HttpHeader requestHeader,
										   HttpHeader responseHeader,
										   char * data,
										   int len) {

		while (checkIf302(responseHeader)) {

			string locStr = responseHeader["Location"];
			Url loc(locStr);
			int contentLength = responseHeader.getContentLength();
			consume(socket, contentLength);

			string path = loc.getPath();
			requestHeader.setPart2(path);
				
			requestHeader.setHeaderField("Host", loc.getAddress());
			sendRequestPacket(socket, requestHeader, data, len);
			responseHeader = readResponseHeader(socket);
		}
		return responseHeader;
	}
}
