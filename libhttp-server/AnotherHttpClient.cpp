#include <string>
#include <liboslayer/Text.hpp>
#include <liboslayer/Timer.hpp>
#include <liboslayer/Logger.hpp>
#include "AnotherHttpClient.hpp"
#include "FixedTransfer.hpp"
#include "DataSink.hpp"
#include "StringDataSink.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getLogger(__FILE__);

	/**
	 * @brief OnResponseHeaderListener
	 */
	OnHttpResponseListener::OnHttpResponseListener() {
	}

	OnHttpResponseListener::~OnHttpResponseListener() {
	}

	AutoRef<DataSink> OnHttpResponseListener::getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	DataTransfer * OnHttpResponseListener::createDataTransfer(HttpHeader & header, AutoRef<DataSink> sink) {
		if (header.isChunkedTransfer()) {
			return new ChunkedTransfer(sink);
		}
		else if (header.getContentLength() > 0) {
			FixedTransfer * transfer = new FixedTransfer(sink, header.getContentLength());
			return transfer;
		}
		return NULL;
	}
	void OnHttpResponseListener::onResponseHeader(HttpResponse & response, UTIL::AutoRef<UserData> userData) {
		response.setTransfer(AutoRef<DataTransfer>(createDataTransfer(response.getHeader(), getDataSink())));
	}

	/**
	 * @brief AnotherHttpClient
	 */
    
    AnotherHttpClient::AnotherHttpClient() : debug(false), connection(NULL), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false), responseListener(NULL), connectionTimeout(0), recvTimeout(0), followRedirect(false) {
        
    }

	AnotherHttpClient::AnotherHttpClient(AutoRef<SocketMaker> socketMaker) : debug(false), connection(NULL), socketMaker(socketMaker), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false), responseListener(NULL), connectionTimeout(0), recvTimeout(0), followRedirect(false) {
        
    }
    
	AnotherHttpClient::AnotherHttpClient(const Url & url) : debug(false), url(url), connection(NULL), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false), responseListener(NULL), connectionTimeout(0), recvTimeout(0), followRedirect(false) {
	}
    
	AnotherHttpClient::~AnotherHttpClient() {
	}

	void AnotherHttpClient::logd(const string & msg) {
		if (debug) {
			logger->logd(msg);
		}
	}

	void AnotherHttpClient::setDebug(bool debug) {
		this->debug = debug;
	}
    
    void AnotherHttpClient::reconnect() {
        closeConnection();
        connect();
    }
	void AnotherHttpClient::connect() {

		if (!connection) {
			string remoteHost = url.getHost();
			int remotePort = url.getIntegerPort();

			if (!socketMaker.nil()) {
				socket = socketMaker->make(url.getProtocol(), OS::InetAddress(remoteHost, remotePort));
			} else {
				socket = AutoRef<Socket>(new Socket(OS::InetAddress(remoteHost, remotePort)));
			}
			
			if (connectionTimeout > 0) {
				socket->connect(connectionTimeout);
			} else {
				socket->connect();
			}

			if (recvTimeout > 0) {
				socket->setRecvTimeout(recvTimeout);
			}

			connection = new Connection(socket);
            connection->registerSelector(selector, Selector::READ | Selector::WRITE);
		}
	}
    void AnotherHttpClient::closeConnection() {
        
        if (connection) {
            
            connection->unregisterSelector(selector, Selector::READ | Selector::WRITE);
            
            delete connection;
            connection = NULL;
            
            socket->close();
            socket = NULL;
        }
    }
    
    void AnotherHttpClient::setUrl(const Url & url) {
        this->url = url;
    }

	void AnotherHttpClient::setRequest(const std::string & method, const UTIL::LinkedStringMap & additionalHeaderFields) {
		HttpRequestHeader & header = request.getHeader();
        header.setMethod(method);
        header.setPath(url.getPathAndQuery());
        header.setProtocol("HTTP/1.1");

		header.appendHeaderFields(additionalHeaderFields);

        header.setHost(url.getAddress());
	}
    void AnotherHttpClient::setRequestWithFixedTransfer(const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer, size_t size) {

		setRequest(method, additionalHeaderFields);
        
        setFixedTransfer(transfer, size);
    }

	void AnotherHttpClient::setRequestWithChunkedTransfer(const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer) {

		setRequest(method, additionalHeaderFields);
		
        setChunkedTransfer(transfer);
	}
    
    void AnotherHttpClient::setFixedTransfer(AutoRef<DataTransfer> transfer, size_t size) {
        HttpRequestHeader & header = request.getHeader();
        if (transfer.nil()) {
            header.setContentLength(0);
        } else {
			request.getHeader().setContentLength(size);
            request.setTransfer(transfer);
        }
    }

	void AnotherHttpClient::setChunkedTransfer(AutoRef<DataTransfer> transfer) {
        HttpRequestHeader & header = request.getHeader();
		header.setChunkedTransfer(true);
		request.setTransfer(transfer);
    }
    
	void AnotherHttpClient::execute() {

		try {
            
            while (!interrupted) {
                
                clearStates();
				connect();
                
                response.clear();
                responseHeaderReader.clear();
                
                communicate();
                
                if (needRedirect()) {
                    handleRedirect();
                    continue;
                } else {
                    break;
                }
            }

		} catch (IOException & e) {
            if (responseListener) {
                responseListener->onError(e, userData);
            }
		}

        closeConnection();
	}
    
    void AnotherHttpClient::communicate() {
		TimeoutChecker readTimeoutChecker(recvTimeout);
        while (!interrupted) {
			if (selector.select(100) > 0) {
				if (connection->isWritableSelected(selector)) {
					sendRequestHeader();
					sendRequestContent();
				}
				if (connection->isReadableSelected(selector)) {
					do {
						recvResponseHeader();
						recvResponseContent();
					} while (connection->socket()->pending() > 0);
					readTimeoutChecker.reset();
				}
				if (complete) {
					break;
				}
			}
			if (readTimeoutChecker.timeout() > 0 && readTimeoutChecker.trigger()) {
				throw Exception("recv timeout - " + Text::toString(recvTimeout) + " ms.");
			}
        }
    }

	void AnotherHttpClient::interrupt() {
		interrupted = true;
	}

	void AnotherHttpClient::clear() {

        closeConnection();
        
        request.clear();
        response.clear();
        responseHeaderReader.clear();

        clearStates();
	}
    
    void AnotherHttpClient::clearStates() {
        requestHeaderSent = false;
        responseHeaderReceived = false;
        readable = false;
        interrupted = false;
        complete = false;
    }
    
	void AnotherHttpClient::sendRequestHeader() {

		if (!requestHeaderSent) {
			string header = request.getHeader().toString();
			connection->send(header.c_str(), header.length());
			requestHeaderSent = true;
		}
	}
	void AnotherHttpClient::sendRequestContent() {

		if (!requestHeaderSent || readable) {
			return;
		}

        AutoRef<DataTransfer> transfer = request.getTransfer();
		if (transfer.empty()) {
			readable = true;
			return;
		}

		transfer->send(*connection);

		if (transfer->completed()) {
			readable = true;
		}
	}
	void AnotherHttpClient::recvResponseHeader() {

		if (readable && !responseHeaderReceived) {

			char buf;
			connection->recv(&buf, 1);

			responseHeaderReader.read(&buf, 1);

			if (responseHeaderReader.complete()) {
                
                response.setHeader(responseHeaderReader.getHeader());
                
                if (responseListener) {
                    responseListener->onResponseHeader(response, userData);
                }
                
				responseHeaderReceived = true;
			}
		}
	}
	void AnotherHttpClient::recvResponseContent() {

		if (readable && responseHeaderReceived) {
            
            AutoRef<DataTransfer> transfer = response.getTransfer();
			if (transfer.empty()) {
                onResponseTransferDone();
				return;
			}

			transfer->recv(*connection);
			if (transfer->completed()) {
                onResponseTransferDone();
			}
		}
	}
    
    void AnotherHttpClient::onResponseTransferDone() {
        setComplete();
    }
    
    void AnotherHttpClient::setComplete() {
        if (responseListener) {
            AutoRef<DataTransfer> transfer = response.getTransfer();
			AutoRef<DataSink> sink = (transfer.nil() ? AutoRef<DataSink>() : transfer->sink());
            responseListener->onTransferDone(response, sink, userData);
        }
        complete = true;
    }
	void AnotherHttpClient::setConnectionTimeout(unsigned long timeout) {
		this->connectionTimeout = timeout;
	}

	void AnotherHttpClient::setRecvTimeout(unsigned long timeout) {
		this->recvTimeout = timeout;
	}
    
    bool AnotherHttpClient::needRedirect() {
        return followRedirect ? response.getHeader().isRedirection() : false;
    }
    
    void AnotherHttpClient::handleRedirect() {
        string location = response.getHeader().getRedirectionLocation();
        url = Url(location);
        request.getHeader().setPath(url.getPathAndQuery());
        request.getHeader().setHost(url.getAddress());
        
        closeConnection();
    }
    
    void AnotherHttpClient::setFollowRedirect(bool followRedirect) {
        this->followRedirect = followRedirect;
    }
    
    void AnotherHttpClient::setOnHttpResponseListener(OnHttpResponseListener * responseListener) {
        this->responseListener = responseListener;
    }

    Url & AnotherHttpClient::getUrl() {
        return url;
    }
    HttpResponse & AnotherHttpClient::getResponse() {
        return response;
    }

	void AnotherHttpClient::setUserData(AutoRef<UserData> userData) {
		this->userData = userData;
	}
}
