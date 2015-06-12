#ifndef __MULTI_CONN_HPP__
#define __MULTI_CONN_HPP__

#include <vector>
#include <map>
#include "os.hpp"

namespace HTTP {

	class Packet {
	private:
		char * buffer;
		int size;
		int length;

	public:
		Packet(int size);
		Packet(char * buffer, int size);
		virtual ~Packet();

		char * getBuffer();
		int put(char * data, int len);
		int getSize();
		void resize(int size);
		int getLength();
	};


	class OnConnectListener {
	public:
		OnConnectListener() {}
		virtual ~OnConnectListener() {}

		virtual void onConnect(OS::Socket & client) = 0;
	};

	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onReceive(OS::Socket & client, Packet & packet) = 0;
	};

	class OnDisconnectListener
	{
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onDisconnect(OS::Socket & client) = 0;
	};


	class MultiConnServer : public OnConnectListener, public OnReceiveListener, public OnDisconnectListener {
	private:
		int port;

		OS::Selector selector;
		std::map<int, OS::Socket*> clients;
		OS::ServerSocket * server;

		OnConnectListener * onConnectListener;
		OnReceiveListener * onReceiveListener;
		OnDisconnectListener * onDisconnectListener;
		
	public:
		MultiConnServer(int port);
		virtual ~MultiConnServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();

		virtual void onConnect(OS::Socket & client);
		virtual void onReceive(OS::Socket & client, Packet & packet);
		virtual void onDisconnect(OS::Socket & client);

		void setOnConnectListener(OnConnectListener * onConnectListener);
		void setOnReceiveListener(OnReceiveListener * onReceiveListener);
		void setOnDisconnectListener(OnDisconnectListener * onDisconnectListener);
		
	};

}

#endif
