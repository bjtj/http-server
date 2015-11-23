#ifndef DataTransfer_hpp
#define DataTransfer_hpp

#include "Connection.hpp"
#include "Packet.hpp"

namespace HTTP {
    class DataTransfer {
    private:
        bool completed;

    public:
        DataTransfer();
        virtual ~DataTransfer();
        
        virtual void reset() = 0;
        virtual void recv(Packet & packet) = 0;
        virtual void send(Connection & connection) = 0;
		virtual unsigned long long getSize();

        void setCompleted();
        virtual bool isCompleted();

		virtual std::string getString() = 0;
    };
}

#endif
