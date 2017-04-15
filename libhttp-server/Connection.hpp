#ifndef __CONNECTION_HPP__
#define __CONNECTION_HPP__

#include "Packet.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Lifetime.hpp>

namespace HTTP {

	/**
	 * @brief 
	 */
	class Connection : public UTIL::Lifetime {
	private:
		OS::AutoRef<OS::Socket> _socket;
		bool terminateFlag;
		bool _completed;
		Packet _packet;
		int id;
		UTIL::Lifetime _recvLifetime;
		unsigned long _recvTimeout;
        unsigned long _recvCount;
        unsigned long _sendCount;
        unsigned long _sendTryCount;

	public:
		Connection(OS::AutoRef<OS::Socket> socket);
		Connection(OS::AutoRef<OS::Socket> socket, size_t packetSize);
		virtual ~Connection();
		int getId();
		OS::AutoRef<OS::Socket> socket();
		void registerSelector(OS::Selector & selector, unsigned char flags);
		void unregisterSelector(OS::Selector & selector, unsigned char flags);
		bool isReadable(OS::Selector & selector);
		bool isWritable(OS::Selector & selector);
		void negotiate();
		int recv(char * buffer, size_t size);
		int send(const char * data, size_t len);
		void close();
		bool isClosed();
		void flagTerminate(bool flag);
		bool isTerminateFlaged();
		bool & completed();
		Packet & read();
		Packet & packet();
		OS::InetAddress getRemoteAddress();
		OS::InetAddress getLocalAddress();
		UTIL::Lifetime & recvLifetime();
		unsigned long getRecvTimeout();
		void setRecvTimeout(unsigned long timeout);
		bool expiredRecvTimeout();
        unsigned long recvCount();
        unsigned long sendCount();
        unsigned long sendTryCount();
	};
}

#endif
