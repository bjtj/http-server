#ifndef Communication_hpp
#define Communication_hpp

#include "Connection.hpp"

namespace HTTP {
    
    class Communication {
    private:
    public:
        Communication();
        virtual ~Communication();
		virtual bool isReadable();
		virtual bool isWritable();
        virtual void onConnected(OS::AutoRef<Connection> connection) = 0;
		virtual bool onReceivable(OS::AutoRef<Connection> connection) = 0;
        virtual bool onWriteable(OS::AutoRef<Connection> connection) = 0;
        virtual void onDisconnected(OS::AutoRef<Connection> connection) = 0;
        virtual bool isCommunicationCompleted() = 0;
    };
    
}

#endif
