#ifndef __HTTP_CLIENT_HPP__
#define __HTTP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/StringElement.hpp>
#include <map>
#include <string>
#include "Url.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"

namespace HTTP {

    template <typename T>
	class HttpClient;
    
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

		virtual void onResponse(HttpClient<T> & httpClient,
                                HttpHeader & responseHeader,
                                OS::Socket & socket,
                                T userData) = 0;
	};
    
    /**
     * @brief http request status
     */
    class HttpRequestStatus {
    private:
        int status;
    public:
        static const int IDLE_STATUS = 0;
        static const int CONNECTING_STATUS = 1;
        static const int SEND_REQUEST_HEADER_STATUS = 2;
        static const int SEND_REQUEST_CONTENT_STATUS = 3;
        static const int RECV_RESPONSE_HEADER_STATUS = 4;
        static const int RECV_RESPONSE_CONTENT_STATUS = 5;
        static const int DONE_STATUS = 6;
        static const int ERROR_STATUS = 7;
    public:
        HttpRequestStatus() : status(IDLE_STATUS) {}
        virtual ~HttpRequestStatus() {}
        void setStatus(int status) {this->status = status;}
        int getStatus() {return status;}
        void operator= (int status) {
            this->status = status;
        }
        bool operator== (int status) const {
            return this->status == status;
        }
        static std::string toString(int status) {
            switch (status) {
                case IDLE_STATUS:
                    return "IDLE_STATUS";
                case CONNECTING_STATUS:
                    return "CONNECTING_STATUS";
                case SEND_REQUEST_HEADER_STATUS:
                    return "SEND_REQUEST_HEADER_STATUS";
                case SEND_REQUEST_CONTENT_STATUS:
                    return "SEND_REQUEST_CONTENT_STATUS";
                case RECV_RESPONSE_HEADER_STATUS:
                    return "RECV_RESPONSE_HEADER_STATUS";
                case RECV_RESPONSE_CONTENT_STATUS:
                    return "RECV_RESPONSE_CONTENT_STATUS";
                case DONE_STATUS:
                    return "DONE_STATUS";
                case ERROR_STATUS:
                    return "ERROR_STATUS";
                default:
                    break;
            }
            return "";
        }
    };
    
    
    /**
     * @brief http client
     */
    template <typename T>
    class HttpClientPollListener {
    private:
    public:
        HttpClientPollListener() {}
        virtual ~HttpClientPollListener() {}
        virtual void onResponseHeader(const HttpHeader & responseHeader, T userData) = 0;
        virtual void onResponseDataChunk(const char * data, size_t len, T userData) = 0;
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
        OS::Selector selector;
        
        HttpClientPollListener<T> * pollListener;
        HttpRequestStatus status;
        HttpHeader requestHeader;
        HttpHeader responseHeader;
        HttpHeaderReader responseHeaderReader;
        Url url;
        char * data;
        size_t dataLen;
		size_t writeLen;
        size_t readLen;
        size_t readTotalLen;
        T userData;
		
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
					 UTIL::StringMap & additionalHeaderFields,
					 const char * data, size_t len, T userData);
        
        void requestStart(const Url & url, const std::string & method,
                          const UTIL::StringMap & additionalHeaderFields,
                          const char * data, size_t len, T userData);
        
        void poll(unsigned long timeout);
        void setHttpClientPollListener(HttpClientPollListener<T> * pollListener);

	private:
		void disconnect(OS::Socket * socket);
		HttpHeader makeRequestHeader(const std::string & method,
									 const std::string & path,
									 const std::string & protocol,
									 const std::string & targetHost);
		void sendRequestPacket(OS::Socket & socket, HttpHeader & header, const char * buffer, size_t len);
        void sendRequestHeaderWithContentLength(OS::Socket & socket, HttpHeader & header, size_t contentLength);
        void sendRequestContent(OS::Socket & socket, const char * content, size_t contentLength);
		HttpHeader readResponseHeader(OS::Socket & socket);
		bool checkIf302(HttpHeader & responseHeader);
		size_t consume(OS::Socket & socket, size_t length);
		HttpHeader processRedirect(OS::Socket & socket,
								   HttpHeader requestHeader,
								   HttpHeader responseHeader,
								   const char * data,
								   size_t len);
	};
    
    
    template<typename T>
    HttpClient<T>::HttpClient() :
    sem(1), responseHandler(NULL), socket(NULL), followRedirect(false), pollListener(NULL),
    data(NULL), dataLen(0), writeLen(0), readLen(0), readTotalLen(0) {
        
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
        UTIL::StringMap empty;
        request(url, "GET", empty, NULL, 0, userData);
    }
    
    template<typename T>
    void HttpClient<T>::request(Url & url, std::string method, const char * data, size_t len, T userData) {
        UTIL::StringMap empty;
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
                                UTIL::StringMap & additionalHeaderFields,
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
            
            // auto disconnect will remove socket
        }
    }
    
    template<typename T>
    void HttpClient<T>::requestStart(const Url & url,
                                const std::string & method,
                                const UTIL::StringMap & additionalHeaderFields,
                                const char * data,
                                size_t len,
                                T userData) {
        
        if (!socket) {
            
            this->url = url;
            if (len > 0) {
                this->data = (char*)malloc(len);
                memcpy(this->data, data, len);
            } else {
                this->data = NULL;
            }
            this->dataLen = len;
            
            this->requestHeader = makeRequestHeader(method, url.getPath(), httpProtocol, url.getAddress());
            this->requestHeader.appendHeaderFields(additionalHeaderFields);
            
            this->writeLen = 0;
            this->readLen = 0;
            this->readTotalLen = 0;
            this->userData = userData;
            
            this->status = HttpRequestStatus::CONNECTING_STATUS;
        }
    }
    
    template<typename T>
    void HttpClient<T>::poll(unsigned long timeout) {
        
//        printf("status: %s\n", HttpRequestStatus::toString(status.getStatus()).c_str());
        switch (status.getStatus()) {
            case HttpRequestStatus::IDLE_STATUS:
            {
                // idle means do nothing
            }
                break;
            case HttpRequestStatus::CONNECTING_STATUS:
            {
                socket = connect(url);
                if (socket) {
                    socket->registerSelector(selector);
                    status = HttpRequestStatus::SEND_REQUEST_HEADER_STATUS;
                } else {
                    status = HttpRequestStatus::ERROR_STATUS;
                }
            }
                break;
            case HttpRequestStatus::SEND_REQUEST_HEADER_STATUS:
            {
                sendRequestHeaderWithContentLength(*socket, requestHeader, dataLen);
                status = HttpRequestStatus::SEND_REQUEST_CONTENT_STATUS;
            }
                break;
            case HttpRequestStatus::SEND_REQUEST_CONTENT_STATUS:
            {
                sendRequestContent(*socket, data, dataLen);
                status = HttpRequestStatus::RECV_RESPONSE_HEADER_STATUS;
            }
                break;
            case HttpRequestStatus::RECV_RESPONSE_HEADER_STATUS:
            {
                char ch;
                if (selector.select(timeout) <= 0) {
                    break;
                }
                int len = socket->recv(&ch, sizeof(char));
                if (len < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
                responseHeaderReader.read(&ch, sizeof(char));
                if (responseHeaderReader.complete()) {
                    responseHeader = responseHeaderReader.getHeader();
                    if (checkIf302(responseHeader)) {
                        url.setPath(responseHeader["Location"]);
                        status = HttpRequestStatus::SEND_REQUEST_HEADER_STATUS;
                    } else if (responseHeader.getContentLength() > 0) {
                        readTotalLen = responseHeader.getContentLength();
                        if (pollListener) {
                            pollListener->onResponseHeader(responseHeader, userData);
                        }
                        status = HttpRequestStatus::RECV_RESPONSE_CONTENT_STATUS;
                    } else {
                        if (pollListener) {
                            pollListener->onResponseHeader(responseHeader, userData);
                        }
                        status = HttpRequestStatus::DONE_STATUS;
                    }
                }
            }
                break;
            case HttpRequestStatus::RECV_RESPONSE_CONTENT_STATUS:
            {
                char buffer[4096] = {0,};
                if (selector.select(timeout) <= 0) {
                    break;
                }
                int len = socket->recv(buffer, sizeof(buffer));
                if (len < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
                
                readLen += len;
                if (pollListener) {
                    pollListener->onResponseDataChunk(buffer, len, userData);
                }
                
                if (readLen >= readTotalLen) {
                    status = HttpRequestStatus::DONE_STATUS;
                }
            }
                break;
            case HttpRequestStatus::DONE_STATUS:
                disconnect();
                status = HttpRequestStatus::IDLE_STATUS;
                break;
            case HttpRequestStatus::ERROR_STATUS:
                disconnect();
                status = HttpRequestStatus::IDLE_STATUS;
                break;
            default:
                break;
        }
    }
    
    template<typename T>
    void HttpClient<T>::setHttpClientPollListener(HttpClientPollListener<T> * pollListener) {
        this->pollListener = pollListener;
    }
    
    template<typename T>
    HttpHeader HttpClient<T>::makeRequestHeader(const std::string & method, const std::string & path, const std::string & protocol, const std::string & targetHost) {
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
    void HttpClient<T>::sendRequestHeaderWithContentLength(OS::Socket & socket, HttpHeader & header, size_t contentLength) {
        header.setContentLength((int)contentLength);
        std::string headerStr = header.toString();
        socket.send(headerStr.c_str(), headerStr.length());
    }
    
    template<typename T>
    void HttpClient<T>::sendRequestContent(OS::Socket & socket, const char * content, size_t contentLength) {
        if (content && contentLength > 0) {
            socket.send(content, contentLength);
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
            throw OS::IOException("read header error", -1, 0);
        }
        return headerReader.getHeader();
    }
    
    template<typename T>
    bool HttpClient<T>::checkIf302(HttpHeader & responseHeader) {
        int statusCode = UTIL::Text::toInt(responseHeader.getPart2());
        return (statusCode == 302);
    }
    
    template<typename T>
    size_t HttpClient<T>::consume(OS::Socket & socket, size_t length) {
        char buffer[1024] = {0,};
        int len;
        size_t total = 0;
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
