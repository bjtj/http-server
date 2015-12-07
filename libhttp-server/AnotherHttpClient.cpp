#include "AnotherHttpClient.hpp"

#include <liboslayer/Text.hpp>
#include <string>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief OnResponseHeaderListener
	 */
	OnResponseListener::OnResponseListener() {
	}

	OnResponseListener::~OnResponseListener() {
	}

	/**
	 * @brief AnotherHttpClient
	 */
    
    AnotherHttpClient::AnotherHttpClient() : connection(NULL), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false), responseListener(NULL), followRedirect(false) {
        
    }
    
	AnotherHttpClient::AnotherHttpClient(const Url & url) : url(url), connection(NULL), socket(NULL), requestHeaderSent(false), responseHeaderReceived(false), readable(false), interrupted(false), complete(false), responseListener(NULL), followRedirect(false) {
	}
    
	AnotherHttpClient::~AnotherHttpClient() {
	}
    
    void AnotherHttpClient::reconnect() {
        closeConnection();
        connect();
    }
	void AnotherHttpClient::connect() {

		if (!connection) {
			string remoteHost = url.getHost();
			int remotePort = url.getIntegerPort();

			socket = new Socket;
            socket->connect(OS::InetAddress(remoteHost, remotePort));

			connection = new Connection(*socket);
            connection->registerSelector(selector);
		}
	}
    
    void AnotherHttpClient::closeConnection() {
        
        if (connection) {
            
            connection->unregisterSelector(selector);
            
            delete connection;
            connection = NULL;
            
            socket->close();
            delete socket;
            socket = NULL;
        }
    }
    
    void AnotherHttpClient::setUrl(const Url & url) {
        this->url = url;
    }
    
    void AnotherHttpClient::setRequest(const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer) {
        HttpRequestHeader & header = request.getHeader();
        header.setMethod(method);
        header.setPath(url.getPath());
        header.setProtocol("HTTP/1.1");
        
        for (size_t i = 0; i < additionalHeaderFields.size(); i++) {
            const NameValue & nv = additionalHeaderFields.const_getByIndex(i);
            header[nv.getName()] = nv.getValue();
        }
        
        header.setHost(url.getAddress());
        
        setDataTransfer(transfer);
    }

	void AnotherHttpClient::setChunkedRequest(const string & method, const LinkedStringMap & additionalHeaderFields, ChunkedTransfer * transfer) {
		HttpRequestHeader & header = request.getHeader();
        header.setMethod(method);
        header.setPath(url.getPath());
        header.setProtocol("HTTP/1.1");
        
        for (size_t i = 0; i < additionalHeaderFields.size(); i++) {
            const NameValue & nv = additionalHeaderFields.const_getByIndex(i);
            header[nv.getName()] = nv.getValue();
        }
        
        header.setHost(url.getAddress());
        
        setChunkedTransfer(transfer);
	}
    
    void AnotherHttpClient::setDataTransfer(AutoRef<DataTransfer> transfer) {
        HttpRequestHeader & header = request.getHeader();
        if (transfer.empty()) {
            header.setContentLength(0);
        } else {
			request.getHeader().setContentLength(transfer->getSize());
            request.setTransfer(transfer);
        }
    }

	void AnotherHttpClient::setChunkedTransfer(ChunkedTransfer * transfer) {
        HttpRequestHeader & header = request.getHeader();
		header.setChunkedTransfer(true);
		request.setTransfer(transfer);
        
    }
    
	void AnotherHttpClient::execute() {

		try {
            
            while (!interrupted) {
                
                clearStates();
                connect();
                
                AutoRef<DataTransfer> transfer = request.getTransfer();
                if (!transfer.empty()) {
                    transfer->reset();
                }
                
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

		} catch (IOException e) {
            if (responseListener) {
                responseListener->onError(e, userData);
            }
		}

        closeConnection();
	}
    
    void AnotherHttpClient::communicate() {
        
        while (!interrupted) {
            
            if (selector.select(100) > 0) {
                
                if (connection->isWritableSelected(selector)) {
                    sendRequestHeader();
                    sendRequestContent();
                }
                
                if (connection->isReadableSelected(selector)) {
                    recvResponseHeader();
                    recvResponseContent();
                }
                
                if (complete) {
                    break;
                }
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

		if (transfer->isCompleted()) {
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

			Packet & packet = connection->read();
			transfer->recv(packet);
			if (transfer->isCompleted()) {
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
            responseListener->onTransferDone(response, &transfer, userData);
        }
        complete = true;
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
    
    void AnotherHttpClient::setOnResponseListener(OnResponseListener * responseListener) {
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