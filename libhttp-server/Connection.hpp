#ifndef __CONNECTION_HPP__
#define __CONNECTION_HPP__

#include "Packet.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Lifetime.hpp>

namespace http {

	/**
	 * @brief 
	 */
	class Connection : public osl::Lifetime {
	private:
		osl::AutoRef<osl::Socket> _socket;
		bool terminateFlag;
		bool _completed;
		Packet _packet;
		int id;
		osl::Lifetime _recvLifetime;
		unsigned long _recvTimeout;
        unsigned long _recvCount;
        unsigned long _sendCount;
        unsigned long _sendTryCount;

	public:
		Connection(osl::AutoRef<osl::Socket> socket);
		Connection(osl::AutoRef<osl::Socket> socket, size_t packetSize);
		virtual ~Connection();
		int getId();
		osl::AutoRef<osl::Socket> socket();
		void registerSelector(osl::Selector & selector, unsigned char flags);
		void unregisterSelector(osl::Selector & selector, unsigned char flags);
		bool isReadable(osl::Selector & selector);
		bool isWritable(osl::Selector & selector);
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
		osl::InetAddress getRemoteAddress();
		osl::InetAddress getLocalAddress();
		osl::Lifetime & recvLifetime();
		unsigned long getRecvTimeout();
		void setRecvTimeout(unsigned long timeout);
		bool expiredRecvTimeout();
        unsigned long recvCount();
        unsigned long sendCount();
        unsigned long sendTryCount();
	};
}

#endif
