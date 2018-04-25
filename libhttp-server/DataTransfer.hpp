#ifndef DataTransfer_hpp
#define DataTransfer_hpp

#include <liboslayer/AutoRef.hpp>
#include "Connection.hpp"
#include "Packet.hpp"
#include "DataSource.hpp"
#include "DataSink.hpp"

namespace http {
	
    class DataTransfer {
    private:
        bool _completed;
		osl::AutoRef<DataSource> _source;
		osl::AutoRef<DataSink> _sink;

    public:
        DataTransfer();
		DataTransfer(osl::AutoRef<DataSource> source);
		DataTransfer(osl::AutoRef<DataSink> sink);
        virtual ~DataTransfer();
		virtual void recv(osl::AutoRef<Connection> connection) = 0;
        virtual void send(osl::AutoRef<Connection> connection) = 0;
		virtual unsigned long long size();
        void complete();
        virtual bool completed();
		osl::AutoRef<DataSource> & source();
		osl::AutoRef<DataSink> & sink();
    };
}

#endif
