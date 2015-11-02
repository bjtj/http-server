#ifndef __HTTP_CLIENT_HPP__
#define __HTTP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <map>
#include <string>
#include "Url.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"

namespace HTTP {

    template <typename T>
	class HttpClient;
    
    class StringMap : public std::map<std::string, std::string> {
    public:
        StringMap() {}
        virtual ~StringMap() {}
    };
    
    /**
     * @brief http response dump utility
     */
    class HttpResponseDump {
    private:
    public:
        HttpResponseDump();
        virtual ~HttpResponseDump();
        static std::string dump(HttpHeader & responseHeader, OS::Socket & socket);
    };
    
	/**
	 * @brief http response handler
	 * it'll be set to http client
	 */
    template <typename T>
	class HttpResponseHandler {
	private:
	public:
		HttpResponseHandler() {}
		virtual ~HttpResponseHandler() {}

		virtual void onResponse(HttpClient<T> & httpClient, HttpHeader & responseHeader, OS::Socket & socket, T userData) = 0;
	};

	/**
	 * @brief http client
	 */
    template <typename T>
	class HttpClient {
	private:
        OS::Semaphore sem;
		std::string httpProtocol;
		HttpResponseHandler<T> * responseHandler;
		OS::Socket * socket;
		std::map<std::string, std::string> defaultHeaderFields;
		bool followRedirect;
		
	public:
		HttpClient();
		virtual ~HttpClient();

		OS::Socket *  connect(Url & url);
		void disconnect();
		void setFollowRedirect(bool followRedirect);
		void setHttpResponseHandler(HttpResponseHandler<T> * responseHandler);
		void request(Url & url, T userData);
		void request(Url & url, std::string method, const char * data, size_t len, T userData);
		void request(Url & url, std::string method,
					 StringMap & additionalHeaderFields,
					 const char * data, size_t len, T userData);

	private:
		void disconnect(OS::Socket * socket);
		HttpHeader makeRequestHeader(std::string method,
									 std::string path,
									 std::string protocol,
									 std::string targetHost);
		void sendRequestPacket(OS::Socket & socket, HttpHeader & header, const char * buffer, size_t len);
		HttpHeader readResponseHeader(OS::Socket & socket);
		bool checkIf302(HttpHeader & responseHeader);
		int consume(OS::Socket & socket, size_t length);
		HttpHeader processRedirect(OS::Socket & socket,
								   HttpHeader requestHeader,
								   HttpHeader responseHeader,
								   const char * data,
								   size_t len);
	};
    
    
    template<typename T>
    HttpClient<T>::HttpClient() : sem(1), responseHandler(NULL), socket(NULL), followRedirect(false) {
        httpProtocol = "HTTP/1.1";
        defaultHeaderFields["User-Agent"] = "Cross-Platform/0.1 HTTP/1.1 HttpClient/0.1";
    }
    
    template<typename T>
    HttpClient<T>::~HttpClient() {
    }

	template<typename T>
    OS::Socket * HttpClient<T>::connect(Url & url) {
        OS::Socket * socket = new OS::Socket(url.getHost().c_str(), url.getIntegerPort());
        socket->connect();
        return socket;
    }

	template<typename T>
    void HttpClient<T>::disconnect() {
        sem.wait();
        disconnect(this->socket);
        this->socket = NULL;
        sem.post();
    }
    
    template<typename T>
    void HttpClient<T>::disconnect(OS::Socket * socket) {
		if (socket) {
			socket->close();
			delete socket;
		}
    }
    
    template<typename T>
    void HttpClient<T>::setFollowRedirect(bool followRedirect) {
        this->followRedirect = followRedirect;
    }
    
    template<typename T>
    void HttpClient<T>::setHttpResponseHandler(HttpResponseHandler<T> * responseHandler) {
        this->responseHandler = responseHandler;
    }
    
    template<typename T>
    void HttpClient<T>::request(Url & url, T userData) {
        std::map<std::string, std::string> empty;
        request(url, "GET", empty, NULL, 0, userData);
    }
    
    template<typename T>
    void HttpClient<T>::request(Url & url, std::string method, const char * data, size_t len, T userData) {
        StringMap empty;
        request(url, method, empty, data, len, userData);
    }
    
    template<typename T>
    class AutoDisconnect {
    private:
        HttpClient<T> * client;
    public:
        AutoDisconnect(HttpClient<T> * client) : client(client) {
        }
        virtual ~AutoDisconnect() {
            client->disconnect();
        }
    };
    
    template<typename T>
    void HttpClient<T>::request(Url & url,
                                std::string method,
                                StringMap & additionalHeaderFields,
                                const char * data,
                                size_t len,
                                T userData) {
        
        if (!socket) {
            
            HttpHeader requestHeader =
            makeRequestHeader(method, url.getPath(), httpProtocol, url.getAddress());
            requestHeader.appendHeaderFields(additionalHeaderFields);
            
            socket = connect(url);
            AutoDisconnect<T> autoDisconnect(this);
            
            sendRequestPacket(*socket, requestHeader, data, len);
            
            HttpHeader responseHeader = readResponseHeader(*socket);
            if (followRedirect && checkIf302(responseHeader)) {
                responseHeader = processRedirect(*socket, requestHeader, responseHeader, data, len);
            }
            
            if (responseHandler) {
                responseHandler->onResponse(*this, responseHeader, *socket, userData);
            }
        }
    }
    
    template<typename T>
    HttpHeader HttpClient<T>::makeRequestHeader(std::string method, std::string path, std::string protocol, std::string targetHost) {
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
    
    template<typename T>
    void HttpClient<T>::sendRequestPacket(OS::Socket & socket, HttpHeader & header, const char * buffer, size_t len) {
        header.setContentLength((int)len);
        std::string headerStr = header.toString();
        socket.send(headerStr.c_str(), headerStr.length());
        if (buffer && len > 0) {
            socket.send(buffer, len);
        }
    }
    
    template<typename T>
    HttpHeader HttpClient<T>::readResponseHeader(OS::Socket & socket) {
        HttpHeaderReader headerReader;
        char buffer;
        int len;
        while (!headerReader.complete() && (len = socket.recv(&buffer, 1)) > 0) {
            headerReader.read(&buffer, 1);
        }
        if (!headerReader.complete()) {
            throw -1;
        }
        return headerReader.getHeader();
    }
    
    template<typename T>
    bool HttpClient<T>::checkIf302(HttpHeader & responseHeader) {
        int statusCode = UTIL::Text::toInt(responseHeader.getPart2());
        return (statusCode == 302);
    }
    
    template<typename T>
    int HttpClient<T>::consume(OS::Socket & socket, size_t length) {
        char buffer[1024] = {0,};
        int len;
        int total = 0;
        while ((len = socket.recv(buffer, sizeof(buffer))) > 0) {
            std::string str(buffer, len);
            total += len;
            if (total >= length) {
                break;
            }
        }
        return total;
    }
    
    template<typename T>
    HttpHeader HttpClient<T>::processRedirect(OS::Socket & socket,
                                              HttpHeader requestHeader,
                                              HttpHeader responseHeader,
                                              const char * data,
                                              size_t len) {
        
        while (checkIf302(responseHeader)) {
            
            std::string locStr = responseHeader["Location"];
            Url loc(locStr);
            int contentLength = responseHeader.getContentLength();
            consume(socket, contentLength);
            
            std::string path = loc.getPath();
            requestHeader.setPart2(path);
            
            requestHeader.setHeaderField("Host", loc.getAddress());
            sendRequestPacket(socket, requestHeader, data, len);
            responseHeader = readResponseHeader(socket);
        }
        return responseHeader;
    }
}

#endif
