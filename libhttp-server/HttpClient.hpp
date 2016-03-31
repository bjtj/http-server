#ifndef __HTTP_CLIENT_HPP__
#define __HTTP_CLIENT_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/StringElement.hpp>
#include <map>
#include <string>
#include "Url.hpp"
#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"
#include "ChunkedReader.hpp"

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
        static std::string dump(const HttpHeader & responseHeader, OS::Socket & socket);
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
		static const int RECV_RESPONSE_READ_CHUNK_SIZE_STATUS = 6;
		static const int RECV_RESPONSE_READ_CHUNK_DATA_STATUS = 7;
		static const int RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS = 8;
        static const int DONE_STATUS = 9;
        static const int ERROR_STATUS = 10;
        
    public:
        HttpRequestStatus() : status(IDLE_STATUS) {}
        virtual ~HttpRequestStatus() {}
        
        void setStatus(int status) {this->status = status;}
        int getStatus() {return status;}
        void operator= (int status) {
            this->status = status;
        }
        bool operator!= (int status) const {
            return this->status != status;
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
				case RECV_RESPONSE_READ_CHUNK_SIZE_STATUS:
					return "RECV_RESPONSE_READ_CHUNK_SIZE_STATUS";
				case RECV_RESPONSE_READ_CHUNK_DATA_STATUS:
					return "RECV_RESPONSE_READ_CHUNK_DATA_STATUS";
				case RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS:
					return "RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS";
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
        virtual void onRequestHeader(HttpClient<T> & httpClient, const HttpHeader & requestHeader, T userData) = 0;
        virtual void onResponseHeader(HttpClient<T> & httpClient, const HttpHeader & responseHeader, T userData) = 0;
        virtual void onResponseDataChunk(HttpClient<T> & httpClient, const HttpHeader & responseHeader, const char * data, size_t len, T userData) = 0;
		virtual void onComplete(HttpClient<T> & httpClient, T userData) = 0;
		virtual void onError(HttpClient<T> & httpClient, T userData) = 0;
        
        
    };

	/**
	 * @brief http client
	 */
    
    template <typename T>
	class HttpClient {
        
	private:
        
        OS::Semaphore sem;
		std::string httpProtocol;
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
        T userData;
		ChunkedReaderBuffer chunkedBuffer;
        ReadCounter contentBuffer;
		ReadCounter consumeBuffer;
        std::string stringBuffer;
        ChunkedBuffer dataBuffer;
		
	public:
        
        
		HttpClient();
		virtual ~HttpClient();

		OS::Socket * connect(Url & url);
		void disconnect();
        bool isFollowRedirect() const;
		void setFollowRedirect(bool followRedirect);
        
        //void request(const std::string & urlString, T userData);
		void request(const Url & url, T userData);
		void request(const Url & url, std::string method, const char * data, size_t len, T userData);
		void request(const Url & url, std::string method,
					 UTIL::StringMap & additionalHeaderFields,
					 const char * data, size_t len, T userData);

        void requestStart(const Url & url, const std::string & method,
                          const UTIL::StringMap & additionalHeaderFields,
                          const char * data, size_t len, T userData);
        
        void poll(unsigned long timeout);
        void setHttpClientPollListener(HttpClientPollListener<T> * pollListener);
        
        int getStatusCode();
        
        int read(unsigned long timeout, char * buffer, size_t size);
        int read(char * buffer, size_t size);
        
        HttpRequestStatus getStatus();
        Url & getUrl();
        HttpHeader & getResponseHeader();
        void clearBuffer();
        std::string & getStringBuffer();
        ChunkedBuffer & getDataBuffer();

        
        
	private:
        
        
		void disconnect(OS::Socket * socket);
		HttpHeader makeRequestHeader(const std::string & method,
									 const std::string & path,
									 const std::string & protocol,
									 const std::string & targetHost);
		void sendRequestPacket(OS::Socket & socket, HttpHeader & header, const char * buffer, size_t len);
        void sendRequestHeaderWithContentLength(OS::Socket & socket, HttpHeader & header, size_t contentLength);
        void sendRequestContent(OS::Socket & socket, const char * content, size_t contentLength);
		bool checkIfRedirect(HttpHeader & responseHeader);
        void processRedirect();
        
        
	};
    
    
    template<typename T>
    HttpClient<T>::HttpClient() :
    sem(1), socket(NULL), followRedirect(false), pollListener(NULL),
    data(NULL), dataLen(0), writeLen(0), contentBuffer(0), consumeBuffer(2) {
        
        httpProtocol = "HTTP/1.1";
        defaultHeaderFields["User-Agent"] = "Cross-Platform/0.1 HTTP/1.1 HttpClient/0.1";
    }
    
    template<typename T>
    HttpClient<T>::~HttpClient() {
        dataBuffer.clear();
    }

	template<typename T>
    OS::Socket * HttpClient<T>::connect(Url & url) {
		
		OS::Socket * socket = new OS::Socket(OS::InetAddress(url.getHost(), url.getIntegerPort()));

		try {
            socket->connect();
		} catch (OS::IOException e) {
			delete socket;
			socket = NULL;
		}
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
    bool HttpClient<T>::isFollowRedirect() const {
        return followRedirect;
    }
    
    template<typename T>
    void HttpClient<T>::setFollowRedirect(bool followRedirect) {
        this->followRedirect = followRedirect;
    }
    
    
    
    template<typename T>
    void HttpClient<T>::request(const Url & url, T userData) {
        UTIL::StringMap empty;
        request(url, "GET", empty, NULL, 0, userData);
    }
    
    template<typename T>
    void HttpClient<T>::request(const Url & url, std::string method, const char * data, size_t len, T userData) {
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
    void HttpClient<T>::request(const Url & url,
                                std::string method,
                                UTIL::StringMap & additionalHeaderFields,
                                const char * data,
                                size_t len,
                                T userData) {
        
        if (!socket) {
            
            requestStart(url, method, additionalHeaderFields, data, len, userData);
            while (status != HttpRequestStatus::IDLE_STATUS) {
                poll(1000);
            }
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
            this->contentBuffer.resetPosition();
            this->userData = userData;
            
            clearBuffer();
            
            this->status = HttpRequestStatus::CONNECTING_STATUS;
        }
    }
    
    template<typename T>
    void HttpClient<T>::poll(unsigned long timeout) {

        switch (status.getStatus()) {
            case HttpRequestStatus::IDLE_STATUS:
            {
                // idle means do nothing
            }
                break;
            case HttpRequestStatus::CONNECTING_STATUS:
            {
                if (url.getProtocol().compare("http")) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
                clearBuffer();
                disconnect();
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
                if (pollListener) {
                    pollListener->onRequestHeader(*this, requestHeader, userData);
                }
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
                if (read(timeout, &ch, 1) < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
                
                responseHeaderReader.read(&ch, 1);
                if (responseHeaderReader.complete()) {
                    responseHeader = responseHeaderReader.getHeader();
					if (pollListener) {
                        pollListener->onResponseHeader(*this, responseHeader, userData);
                    }
                    responseHeaderReader.clear();
                    contentBuffer.resetPosition();

                    if (responseHeader.isChunkedTransfer()) {
						chunkedBuffer.clear();
						status = HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_SIZE_STATUS;
					} else if (responseHeader.getContentLength() > 0) {
                        contentBuffer.setContentSize(responseHeader.getContentLength());
                        status = HttpRequestStatus::RECV_RESPONSE_CONTENT_STATUS;
                    } else {
                        status = HttpRequestStatus::DONE_STATUS;
                    }
                }
            }
                break;
            case HttpRequestStatus::RECV_RESPONSE_CONTENT_STATUS:
            {
                char buffer[4096] = {0,};
                int len;
                if ((len = read(timeout, buffer, contentBuffer.getReadSize(sizeof(buffer)))) < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
                
                contentBuffer.read(len);
                
                if (pollListener) {
                    pollListener->onResponseDataChunk(*this, responseHeader, buffer, len, userData);
                }
                
                if (contentBuffer.completed()) {
                    if (followRedirect && checkIfRedirect(responseHeader)) {
                        processRedirect();
                        status = HttpRequestStatus::CONNECTING_STATUS;
                    } else {
                        status = HttpRequestStatus::DONE_STATUS;
                    }
                }
            }
                break;
			case HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_SIZE_STATUS:
			{
				char ch;
                if (read(timeout, &ch, 1) < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
				chunkedBuffer.readChunkSize(ch);
				if (chunkedBuffer.hasSizeRecognized()) {
					status = HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_DATA_STATUS;
				}
			}
				break;
			case HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_DATA_STATUS:
			{
				if (chunkedBuffer.getChunkSize() == 0) {
					consumeBuffer.resetPosition();
					consumeBuffer.setContentSize(2);
					status = HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS;
					break;
				}
				char buffer[4096] = {0,};
                int len;
                if ((len = read(timeout, buffer, chunkedBuffer.getReadableSize(sizeof(buffer)))) < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
				chunkedBuffer.write(buffer, len);
				if (chunkedBuffer.completeData()) {
					if (pollListener) {
						pollListener->onResponseDataChunk(*this, responseHeader, chunkedBuffer.getChunkData(), chunkedBuffer.getChunkSize(), userData);
					}

					consumeBuffer.resetPosition();
					consumeBuffer.setContentSize(2);
					status = HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS;
				}
			}
				break;
			case HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_DATA_TRAILING_STATUS:
			{
				char ch;
                int len;
                if ((len = read(timeout, &ch, 1)) < 0) {
                    status = HttpRequestStatus::ERROR_STATUS;
                    break;
                }
				consumeBuffer.read(len);
				if (consumeBuffer.completed()) {
					if (chunkedBuffer.getChunkSize() == 0) {
                        if (followRedirect && checkIfRedirect(responseHeader)) {
                            processRedirect();
                            status = HttpRequestStatus::CONNECTING_STATUS;
                        } else {
                            status = HttpRequestStatus::DONE_STATUS;
                        }
					} else {
						chunkedBuffer.clear();
						status = HttpRequestStatus::RECV_RESPONSE_READ_CHUNK_SIZE_STATUS;
					}
				}
			}
				break;
            case HttpRequestStatus::DONE_STATUS:
			{
				if (pollListener) {
                    pollListener->onComplete(*this, userData);
                }
                disconnect();
                clearBuffer();
                status = HttpRequestStatus::IDLE_STATUS;
			}
                break;
            case HttpRequestStatus::ERROR_STATUS:
			{
				if (pollListener) {
                    pollListener->onError(*this, userData);
                }
                disconnect();
                clearBuffer();
                status = HttpRequestStatus::IDLE_STATUS;
			}
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
    int HttpClient<T>::getStatusCode() {
        return UTIL::Text::toInt(responseHeader.getPart2());
    }
    
    template<typename T>
    int HttpClient<T>::read(unsigned long timeout, char * buffer, size_t size) {
        if (selector.select(timeout) > 0) {
            return read(buffer, size);
        }
        return 0;
    }
    
    template<typename T>
    int HttpClient<T>::read(char * buffer, size_t size) {
        try {
            int recvLen = socket->recv(buffer, size);
            return recvLen;
        } catch (OS::IOException e) {
            return -1;
        }
    }
    
    template<typename T>
    HttpRequestStatus HttpClient<T>::getStatus() {
        return status;
    }
    
    template<typename T>
    Url & HttpClient<T>::getUrl() {
        return url;
    }
    
    template<typename T>
    HttpHeader & HttpClient<T>::getResponseHeader() {
        return responseHeader;
    }
    
    template<typename T>
    void HttpClient<T>::clearBuffer() {
        stringBuffer.clear();
        dataBuffer.clear();
    }
    
    template<typename T>
    std::string & HttpClient<T>::getStringBuffer() {
        return stringBuffer;
    }
    
    template<typename T>
    ChunkedBuffer & HttpClient<T>::getDataBuffer() {
        return dataBuffer;
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
            int len = socket.send(content, contentLength);
			if (len != contentLength) {
				// TODO: make it sure whole contents sent
			}
        }
    }
    
    template<typename T>
    bool HttpClient<T>::checkIfRedirect(HttpHeader & responseHeader) {
        int statusCode = UTIL::Text::toInt(responseHeader.getPart2());
        return (statusCode == 301 || statusCode == 302);
    }
    
    template<typename T>
    void HttpClient<T>::processRedirect() {
        url.setUrl(responseHeader["Location"]);
        requestHeader.setPart2(url.getPath());
        requestHeader["Host"] = url.getAddress();
    }
    
    
    /**
     * @brief http response handler
     */
    template <typename T>
    class HttpResponseHandler : public HttpClientPollListener<T> {
    private:
    public:
        HttpResponseHandler() {}
        virtual ~HttpResponseHandler() {}
        virtual void onRequestHeader(HttpClient<T> & httpClient, const HttpHeader & requestHeader, T userData) {
        }
        virtual void onResponseHeader(HttpClient<T> & httpClient, const HttpHeader & responseHeader, T userData) {
        }
        virtual void onResponseDataChunk(HttpClient<T> & httpClient, const HttpHeader & responseHeader, const char * data, size_t len, T userData) {
            int statusCode = httpClient.getStatusCode();
            if (!httpClient.isFollowRedirect() || (statusCode != 301 && statusCode != 302)) {
                httpClient.getStringBuffer().append(data, len);
            }
        }
        virtual void onComplete(HttpClient<T> & httpClient, T userData) {
            onHttpResponse(httpClient, httpClient.getResponseHeader(), httpClient.getStringBuffer(), userData);
        }
        
        virtual void onHttpResponse(HttpClient<T> & httpClient, const HttpHeader & responseHeader, const std::string & content, T userData) = 0;
    };
}

#endif
