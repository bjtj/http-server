#ifndef __FIXED_TRANSFER_HPP__
#define __FIXED_TRANSFER_HPP__

#include "DataTransfer.hpp"
#include "BufferIndicator.hpp"

namespace HTTP {
    
    /**
     * @brief FixedTransfer
     */
    class FixedTransfer : public DataTransfer {
    private:

		BufferIndicator indicator;
        
    public:
		FixedTransfer(UTIL::AutoRef<DataSource> source, size_t size);
		FixedTransfer(UTIL::AutoRef<DataSink> sink, size_t size);
        virtual ~FixedTransfer();
		virtual void recv(Connection & connection);
        virtual void send(Connection & connection);
		virtual unsigned long long size();
    };
}

#endif
