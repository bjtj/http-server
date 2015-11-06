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
                int len = reader.readChunkData(buffer, size);
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
