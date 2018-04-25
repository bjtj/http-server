#ifndef __FIXED_TRANSFER_HPP__
#define __FIXED_TRANSFER_HPP__

#include "DataTransfer.hpp"
#include "BufferIndicator.hpp"

namespace http {
    
    /**
     * @brief FixedTransfer
     */
    class FixedTransfer : public DataTransfer {
    private:
		BufferIndicator indicator;
		char * _buffer;
		size_t _size;
    public:
		FixedTransfer(osl::AutoRef<DataSource> source, size_t size);
        FixedTransfer(osl::AutoRef<DataSource> source, size_t size, size_t fragmentSize);
		FixedTransfer(osl::AutoRef<DataSink> sink, size_t size);
        virtual ~FixedTransfer();
	private:
		void realloc(size_t size);
	public:
		virtual void recv(osl::AutoRef<Connection> connection);
        virtual void send(osl::AutoRef<Connection> connection);
		virtual unsigned long long size();
    };
}

#endif
