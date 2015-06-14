#ifndef __MULTI_CONN_HPP__
#define __MULTI_CONN_HPP__

#include <vector>
#include <map>
#include "os.hpp"

namespace HTTP {

	class MultiConnServer;

	/**
	 * @brief packet
	 */
	class Packet {
	private:
		char * buffer;
		size_t _size;
		int _length;

	public:
		Packet(int size);
		Packet(char * buffer, size_t size);
		virtual ~Packet();

		char * getBuffer();
		int put(char * data, int len);
		size_t size();
		void resize(int size);
		int length();
	};


	/**
	 * @brief on connect listener
	 */
	class OnConnectListener {
	public:
		OnConnectListener() {}
		virtual ~OnConnectListener() {}

		virtual void onConnect(MultiConnServer & server, OS::Socket & client) = 0;
	};

	/**
	 * @brief on receive listener
	 */
	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) = 0;
	};

	/**
	 * @brief on disconnect listener
	 */
	class OnDisconnectListener {
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client) = 0;
	};

	class MultiConnProtocol : public OnConnectListener,
							  public OnReceiveListener,
							  public OnDisconnectListener {
	public:
        MultiConnProtocol() {}
		virtual ~MultiConnProtocol() {}
		virtual void onConnect(MultiConnServer & server, OS::Socket & client) = 0;
		virtual void onReceive(MultiConnServer & server, OS::Socket & client, Packet & packet) = 0;
		virtual void onDisconnect(MultiConnServer & server, OS::Socket & client) = 0;
	};

	/**
	 * @brief multi connection server
	 */
	class MultiConnServer {
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

		virtual bool isDisconnected(OS::Socket & client);
		virtual void disconnect(OS::Socket & client);

		virtual void onConnect(OS::Socket & client);
		virtual void onReceive(OS::Socket & client, Packet & packet);
		virtual void onDisconnect(OS::Socket & client);

		void setOnConnectListener(OnConnectListener * onConnectListener);
		void setOnReceiveListener(OnReceiveListener * onReceiveListener);
		void setOnDisconnectListener(OnDisconnectListener * onDisconnectListener);

		void setProtocol(MultiConnProtocol * protocol);
	};
	
}

#endif
