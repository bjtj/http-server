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
        virtual void onConnected(Connection & connection) = 0;
		virtual void onReceivable(Connection & connection) = 0;
        virtual void onWriteable(Connection & connection) = 0;
        virtual void onDisconnected(Connection & connection) = 0;
        virtual bool isCommunicationCompleted() = 0;
    };
    
}

#endif
