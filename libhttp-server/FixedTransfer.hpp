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
		char * _buffer;
		size_t _size;
    public:
		FixedTransfer(OS::AutoRef<DataSource> source, size_t size);
        FixedTransfer(OS::AutoRef<DataSource> source, size_t size, size_t fragmentSize);
		FixedTransfer(OS::AutoRef<DataSink> sink, size_t size);
        virtual ~FixedTransfer();
	private:
		void realloc(size_t size);
	public:
		virtual void recv(OS::AutoRef<Connection> connection);
        virtual void send(OS::AutoRef<Connection> connection);
		virtual unsigned long long size();
    };
}

#endif
