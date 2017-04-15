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
		OS::AutoRef<DataSource> _source;
		OS::AutoRef<DataSink> _sink;

    public:
        DataTransfer();
		DataTransfer(OS::AutoRef<DataSource> source);
		DataTransfer(OS::AutoRef<DataSink> sink);
        virtual ~DataTransfer();
		virtual void recv(OS::AutoRef<Connection> connection) = 0;
        virtual void send(OS::AutoRef<Connection> connection) = 0;
		virtual unsigned long long size();
        void complete();
        virtual bool completed();
		OS::AutoRef<DataSource> & source();
		OS::AutoRef<DataSink> & sink();
    };
}

#endif
