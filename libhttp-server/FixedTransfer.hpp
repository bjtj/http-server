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
		FixedTransfer(const char * content, size_t size);
		FixedTransfer(const std::string & content);
        virtual ~FixedTransfer();
        ChunkedBuffer & getChunkedBuffer();
        
        virtual void reset();
        virtual void recv(Packet & packet);
        virtual void send(Connection & connection);
		virtual unsigned long long getSize();
        
        virtual std::string getString();
    };
}

#endif