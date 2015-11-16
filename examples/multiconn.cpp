#include <liboslayer/os.hpp>
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

using namespace std;
using namespace OS;
using namespace UTIL;
using namespace HTTP;

static const Logger & logger = LoggerFactory::getDefaultLogger();

// 1. connection manager
//  - add connection
//  - remove connection

// 2. thread way
//  - make thread
//  - make connection session
//  - let it go

// 3. multiplex way
//  - register client fd
//  - bypass read/write states


// Connection
//  - handle recv
//  - handle write
//  - flag Connection closed


class ChunkedTransfer : public DataTransfer {
private:
    ChunkedReaderBuffer readerBuffer;
public:
    ChunkedTransfer() {
    }
    virtual ~ChunkedTransfer() {
    }
    virtual void recv(Packet & packet) {
        char * p = packet.getData();
        for (size_t i = 0; i < packet.getLength(); i++, p++) {
            char ch = *p;
            
            if (!readerBuffer.hasSizeRecognized()) {
                readerBuffer.readChunkSize(ch);
            } else {
                if (readerBuffer.remainingDataBuffer() < readerBuffer.getChunkSize() + 2) {
                    
                    if (readerBuffer.remain()) {
                        readerBuffer.write(p, 1);
                    } else {
                        // ignore \r\n
                    }
                } else {
                    
                    if (readerBuffer.getChunkSize() == 0) {
                        setCompleted();
                    }
                }
                
            }
        }
    }
    virtual void send(Connection & connection) {
    }
};

class FixedTransfer : public DataTransfer {
private:
    ChunkedBuffer chunkedBuffer;
public:
    FixedTransfer() {
    }
    virtual ~FixedTransfer() {
    }
    ChunkedBuffer & getChunkedBuffer() {
        return chunkedBuffer;
    }
    virtual void recv(Packet & packet) {
        chunkedBuffer.write(packet.getData(), packet.getLength());
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }
    virtual void send(Connection & connection) {
        if (chunkedBuffer.remain()) {
            char buffer[1024] = {0,};
            size_t len = chunkedBuffer.read(buffer, sizeof(buffer));
            connection.send(buffer, len);
        }
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }
};

class HttpResponse {
private:
    HttpResponseHeader header;
    DataTransfer * transfer;
    
public:
    HttpResponse() : transfer(NULL) {
    }
    virtual ~HttpResponse() {
        if (transfer) {
            delete transfer;
        }
    }
    HttpResponseHeader & getHeader() {
        return header;
    }
    void setTransfer(DataTransfer * transfer) {
        this->transfer = transfer;
    }
    DataTransfer * getTransfer() {
        return transfer;
    }
};

class HttpRequestHandler {
private:
public:
    
    virtual void onRequest(HttpRequest & request, HttpResponse & response) {
        
        HttpResponseHeader & responseHeader = response.getHeader();
        
        responseHeader.setProtocol("HTTP/1.1");
        responseHeader.setStatusCode(200);
        responseHeader.setMessage("OK");
        responseHeader.setHeaderField("Connection", "close");
        responseHeader.setContentType("text/html");
        
        logger.logv(request.getHeader().toString());
        
        string content = "Method: " + request.getMethod() + " / Path: " + request.getPath() + "\r\n";
        content.append("<form method='post'><input type='text' name='name' /></form>");
        
        FixedTransfer * transfer = new FixedTransfer;
        ChunkedBuffer & cb = transfer->getChunkedBuffer();
        cb.setChunkSize(content.length());
        cb.write(content.c_str(), content.length());
        cb.resetPosition();
        
        response.setTransfer(transfer);
        responseHeader.setContentLength((int)content.length());
        
    }
    
    virtual void onRequestContent(Packet & packet) {
        
        logger.logv(string(packet.getData(), packet.getLength()));
    }
};

class HttpCommunication : public Communication {
private:
	HttpRequest request;
    HttpHeaderReader requestHeaderReader;
    HttpResponse response;
	bool requestHeaderHandled;
    ReadCounter requestContentReadCounter;
	bool writeable;
	bool responseHeaderTransferDone;
	bool responseContentTransferDone;
	bool communicationCompleted;
    
    DataTransfer * requestTransfer;
    
    HttpRequestHandler handler;

public:
	HttpCommunication() : requestHeaderHandled(false), writeable(false), responseHeaderTransferDone(false), responseContentTransferDone(false), communicationCompleted(false), requestTransfer(NULL) {
	}
	virtual ~HttpCommunication() {
        if (requestTransfer) {
            delete requestTransfer;
        }
	}

	virtual void onConnected(Connection & connection) {
		connection.setReadSize(1);
	}

	virtual void onDataReceived(Connection & connection, Packet & packet) {

		readRequestHeaderIfNeed(connection, packet);
		if (requestHeaderReader.complete()) {
            
            if (!requestHeaderHandled) {
                
                onRequestHeader(request);
                requestContentReadCounter.setContentSize(request.getContentLength());
                
            } else {
                
                requestTransfer->recv(packet);
                readRequestContent(packet);
                if (requestTransfer->isCompleted()) {
                    writeable = true;
                }
            }
		}
	}

	void readRequestHeaderIfNeed(Connection & connection, Packet & packet) {

		if (!requestHeaderReader.complete()) {
           
            requestHeaderReader.read(packet.getData(), (int)packet.getLength());

			packet.clear();

			if (requestHeaderReader.complete()) {
                request.setHeader(requestHeaderReader.getHeader());
				connection.resetReadLimit();
			}
		}
	}
    
    void readRequestContent(Packet & packet) {
        string chunk(packet.getData(), packet.getLength());
//        logger.logv("READ: " + chunk);
        
        handler.onRequestContent(packet);
    }

	void onRequestHeader(HttpRequest & request) {
        
        
        if (request.getHeader().isChunkedTransfer()) {
            
            requestTransfer = new ChunkedTransfer;
            
        } else {
            
            if (request.getContentLength() == 0) {
                
                writeable = true;
                
            } else {
                
                FixedTransfer * transfer = new FixedTransfer;
                ChunkedBuffer & cb = transfer->getChunkedBuffer();
                cb.setChunkSize(request.getContentLength());
                requestTransfer = transfer;
                
            }
        }
        
        handler.onRequest(request, response);
        
        requestHeaderHandled = true;
	}

	virtual void onWriteable(Connection & connection) {

		if (!writeable) {
			return;
		}

		if (!responseHeaderTransferDone) {
            
            HttpHeader & header = response.getHeader();
            
            string headerString = header.toString();
            connection.send(headerString.c_str(), (int)headerString.length());
            
            if (!header.isChunkedTransfer() && header.getContentLength() == 0) {
                responseContentTransferDone = true;
            }
            
            responseHeaderTransferDone = true;
		}

		if (!responseContentTransferDone) {
            
            response.getTransfer()->send(connection);
            if (response.getTransfer()->isCompleted()) {
                responseContentTransferDone = true;
            }
		}

		if (responseContentTransferDone) {
			communicationCompleted = true;
		}
	}

	virtual void onDisconnected(Connection & connection) {
	}

	virtual bool isCommunicationCompleted() {
		return communicationCompleted;
	}
};

class HttpCommunicationMaker : public CommunicationMaker {
private:
public:
    HttpCommunicationMaker() {
    }
    virtual ~HttpCommunicationMaker() {
    }
    
    virtual Communication * makeCommunication() {
        return new HttpCommunication;
    }
};

int main(int argc, char * args[]) {

	bool done = false;
    HttpCommunicationMaker hcm;
	ConnectionManager cm(hcm);

	cm.start(8083);

	while (!done) {
		cm.poll(1000);
	}

	cm.stop();

	return 0;
}