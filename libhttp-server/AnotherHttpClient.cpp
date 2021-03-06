#include <string>
#include <liboslayer/File.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Timer.hpp>
#include <liboslayer/Logger.hpp>
#include "AnotherHttpClient.hpp"
#include "FixedTransfer.hpp"
#include "DataSink.hpp"
#include "StringDataSink.hpp"

namespace http {

	using namespace std;
	using namespace osl;

	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));

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
	void OnHttpResponseListener::onResponseHeader(HttpResponse & response, AutoRef<UserData> userData) {
		response.setTransfer(AutoRef<DataTransfer>(createDataTransfer(response.header(), getDataSink())));
	}

	/**
	 * @brief AnotherHttpClient
	 */
    
    AnotherHttpClient::AnotherHttpClient() :
		_debug(false),
		connection(NULL),
		socket(NULL),
		requestHeaderSent(false),
		responseHeaderReceived(false),
		readable(false),
		interrupted(false),
		complete(false),
		responseListener(NULL),
		connectionTimeout(0),
		recvTimeout(0),
		followRedirect(false) {
        
    }

	AnotherHttpClient::AnotherHttpClient(AutoRef<SocketMaker> socketMaker) :
		_debug(false),
		connection(NULL),
		socketMaker(socketMaker),
		socket(NULL),
		requestHeaderSent(false),
		responseHeaderReceived(false),
		readable(false),
		interrupted(false),
		complete(false),
		responseListener(NULL),
		connectionTimeout(0),
		recvTimeout(0),
		followRedirect(false) {
        
    }
    
	AnotherHttpClient::AnotherHttpClient(const Url & url)
		: _debug(false),
		  url(url),
		  connection(NULL),
		  socket(NULL),
		  requestHeaderSent(false),
		  responseHeaderReceived(false),
		  readable(false),
		  interrupted(false),
		  complete(false),
		  responseListener(NULL),
		  connectionTimeout(0),
		  recvTimeout(0),
		  followRedirect(false) {
	}
    
	AnotherHttpClient::~AnotherHttpClient() {
	}

	void AnotherHttpClient::logd(const string & msg) {
		if (_debug) {
			logger->debug(msg);
		}
	}

	void AnotherHttpClient::setDebug(bool debug) {
		_debug = debug;
	}
    
    void AnotherHttpClient::reconnect() {
        close();
        connect();
    }
	void AnotherHttpClient::connect() {

		if (connection.nil() == false) {
            return;
        }
        
        string remoteHost = url.getHost();
        int remotePort = url.getIntegerPort();
        
        if (!socketMaker.nil()) {
            socket = socketMaker->make(url.getProtocol(), osl::InetAddress(remoteHost, remotePort));
        } else {
            socket = AutoRef<Socket>(new Socket(osl::InetAddress(remoteHost, remotePort)));
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
    void AnotherHttpClient::close() {
        
        if (connection.nil()) {
            return;
        }
        
        connection->unregisterSelector(selector, Selector::READ | Selector::WRITE);
        
        connection = NULL;
        
        socket->close();
        socket = NULL;
        
    }
    
    void AnotherHttpClient::setUrl(const Url & url) {
        this->url = url;
    }

	void AnotherHttpClient::setRequest(const std::string & method, const LinkedStringMap & additionalHeaderFields) {
		HttpRequestHeader & header = request.header();
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
        HttpRequestHeader & header = request.header();
        if (transfer.nil()) {
            header.setContentLength(0);
        } else {
			request.setContentLength(size);
            request.setTransfer(transfer);
        }
    }

	void AnotherHttpClient::setChunkedTransfer(AutoRef<DataTransfer> transfer) {
        HttpRequestHeader & header = request.header();
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

        close();
	}
    
    void AnotherHttpClient::communicate() {
		Timeout readTimeout(recvTimeout);
        while (!interrupted) {
			if (selector.select(100) > 0) {
				if (connection->isWritable(selector)) {
					sendRequestHeader();
					sendRequestContent();
				}
				if (connection->isReadable(selector)) {
					do {
						recvResponseHeader();
						recvResponseContent();
					} while (connection->socket()->pending() > 0);
					readTimeout.reset();
				}
				if (complete) {
					break;
				}
			}
			if (readTimeout.value() > 0 && readTimeout.expired()) {
				throw Exception("recv timeout - " + Text::toString(recvTimeout) + " ms.");
			}
        }
    }

	void AnotherHttpClient::interrupt() {
		interrupted = true;
	}

	void AnotherHttpClient::clear() {

        close();
        
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
			string header = request.header().toString();
			connection->send(header.c_str(), header.length());
			requestHeaderSent = true;
		}
	}
	void AnotherHttpClient::sendRequestContent() {

		if (!requestHeaderSent || readable) {
			return;
		}

        AutoRef<DataTransfer> transfer = request.getTransfer();
		if (transfer.nil()) {
			readable = true;
			return;
		}

		transfer->send(connection);

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
                
                response.header() = responseHeaderReader.getHeader();
                
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
			if (transfer.nil()) {
                onResponseTransferDone();
				return;
			}
			transfer->recv(connection);
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
        return followRedirect ? response.isRedirectionStatus() : false;
    }
    
    void AnotherHttpClient::handleRedirect() {
        string location = response.getLocation();
        url = Url(location);
        request.setPath(url.getPathAndQuery());
        request.setHost(url.getAddress());
        
        close();
    }
    
    void AnotherHttpClient::setFollowRedirect(bool followRedirect) {
        this->followRedirect = followRedirect;
    }
    
    void AnotherHttpClient::setOnHttpResponseListener(OnHttpResponseListener * responseListener) {
        this->responseListener = responseListener;
    }

    Url AnotherHttpClient::getUrl() {
        return url;
    }
	
    HttpResponse & AnotherHttpClient::getResponse() {
        return response;
    }

	void AnotherHttpClient::setUserData(AutoRef<UserData> userData) {
		this->userData = userData;
	}
}
