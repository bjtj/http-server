#ifndef __CONNECTION_HPP__
#define __CONNECTION_HPP__

#include "Packet.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>

namespace HTTP {
    class Connection {
    private:
        XOS::Socket & socket;
        bool terminateSignal;
        bool completed;
        Packet packet;
        
    public:
        Connection(XOS::Socket & socket);
        virtual ~Connection();
        int getId();
        void registerSelector(OS::Selector & selector);
		void unregisterSelector(OS::Selector & selector);
        bool isReadableSelected(OS::Selector & selector);
        bool isWritableSelected(OS::Selector & selector);
        int recv(char * buffer, size_t size);
        int send(const char * data, size_t len);
        void close();
        bool isClosed();
        void signalTerminate();
        bool isTerminateSignaled();
        void setCompleted();
        bool isCompleted();
        void setReadSize(size_t readSize);
        void resetReadLimit();
        Packet & read();
    };
}

#endif
