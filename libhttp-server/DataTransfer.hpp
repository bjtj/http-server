#ifndef DataTransfer_hpp
#define DataTransfer_hpp

#include <liboslayer/AutoRef.hpp>
#include "Connection.hpp"
#include "Packet.hpp"
#include "DataSource.hpp"
#include "DataSink.hpp"

namespace HTTP {
	
    class DataTransfer {
    private:
        bool _completed;
		UTIL::AutoRef<DataSource> _source;
		UTIL::AutoRef<DataSink> _sink;

    public:
        DataTransfer();
		DataTransfer(UTIL::AutoRef<DataSource> source);
		DataTransfer(UTIL::AutoRef<DataSink> sink);
        virtual ~DataTransfer();
		virtual void recv(UTIL::AutoRef<Connection> connection) = 0;
        virtual void send(UTIL::AutoRef<Connection> connection) = 0;
		virtual unsigned long long size();
        void complete();
        virtual bool completed();
		UTIL::AutoRef<DataSource> & source();
		UTIL::AutoRef<DataSink> & sink();
    };
}

#endif
