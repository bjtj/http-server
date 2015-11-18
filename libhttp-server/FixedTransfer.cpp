#include "FixedTransfer.hpp"

#include <liboslayer/Logger.hpp>

namespace HTTP {
    
    using namespace std;
    using namespace UTIL;
    
    static const Logger & logger = LoggerFactory::getDefaultLogger();
    
    /**
     * @brief FixedTransfer
     */
    
    FixedTransfer::FixedTransfer(size_t size) {
        chunkedBuffer.setChunkSize(size);
    }
    FixedTransfer::~FixedTransfer() {
    }
    ChunkedBuffer & FixedTransfer::getChunkedBuffer() {
        return chunkedBuffer;
    }
    void FixedTransfer::recv(Packet & packet) {
        chunkedBuffer.write(packet.getData(), packet.getLength());
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }
    void FixedTransfer::send(Connection & connection) {
        if (chunkedBuffer.remain()) {
			int len = connection.send(chunkedBuffer.getChunkData(), chunkedBuffer.remainingDataBuffer());
			chunkedBuffer.setPosition(len);
        }
        
        if (!chunkedBuffer.remain()) {
            setCompleted();
        }
    }
    
    string FixedTransfer::getString() {
        return string(chunkedBuffer.getChunkData(), chunkedBuffer.getChunkSize());
    }
}