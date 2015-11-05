#include <liboslayer/Text.hpp>
#include "HttpClient.hpp"
#include "ChunkedReader.hpp"
#include "Logger.hpp"

using namespace std;;
using namespace OS;
using namespace UTIL;

namespace HTTP {

	static Logger & logger = Logger::getLogger();


	const int HttpRequestStatus::IDLE_STATUS = 0;
    const int HttpRequestStatus::CONNECTING_STATUS = 1;
    const int HttpRequestStatus::SEND_REQUEST_HEADER_STATUS = 2;
    const int HttpRequestStatus::SEND_REQUEST_CONTENT_STATUS = 3;
    const int HttpRequestStatus::RECV_RESPONSE_HEADER_STATUS = 4;
    const int HttpRequestStatus::RECV_RESPONSE_CONTENT_STATUS = 5;
    const int HttpRequestStatus::DONE_STATUS = 6;
    const int HttpRequestStatus::ERROR_STATUS = 7;
    
    
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
        
}
