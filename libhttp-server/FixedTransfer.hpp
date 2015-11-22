#ifndef __FIXED_TRANSFER_HPP__
#define __FIXED_TRANSFER_HPP__

#include "DataTransfer.hpp"
#include "ChunkedReader.hpp"

namespace HTTP {
    
    /**
     * @brief FixedTransfer
     */
    class FixedTransfer : public DataTransfer {
    private:
        
        ChunkedBuffer chunkedBuffer;
        
    public:
        
        FixedTransfer(size_t size);
        virtual ~FixedTransfer();
        ChunkedBuffer & getChunkedBuffer();
        
        virtual void reset();
        virtual void recv(Packet & packet);
        virtual void send(Connection & connection);
        
        virtual std::string getString();
    };
}

#endif