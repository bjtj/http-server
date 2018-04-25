#ifndef Communication_hpp
#define Communication_hpp

#include "Connection.hpp"

namespace http {
    
    class Communication {
    private:
    public:
        Communication();
        virtual ~Communication();
		virtual bool isReadable();
		virtual bool isWritable();
        virtual void onConnected(osl::AutoRef<Connection> connection) = 0;
		virtual bool onReceivable(osl::AutoRef<Connection> connection) = 0;
        virtual bool onWriteable(osl::AutoRef<Connection> connection) = 0;
        virtual void onDisconnected(osl::AutoRef<Connection> connection) = 0;
        virtual bool isCommunicationCompleted() = 0;
    };
    
}

#endif
