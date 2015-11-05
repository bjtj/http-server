#ifndef __MULTI_CONN_HPP__
#define __MULTI_CONN_HPP__

#include <vector>
#include <map>
#include <liboslayer/os.hpp>

namespace HTTP {

	class MultiConn;
	class MultiConnMultiplexServer;
	class MultiConnThreadedServer;

	/**
	 * @brief packet
	 */
	class Packet {
	private:
		char * buffer;
		int _size;
		int _length;

	public:
		Packet(int size);
		Packet(char * buffer, int size);
		virtual ~Packet();

		char * getBuffer();
		int put(char * data, int len);
		int size();
		void resize(int size);
		int length();
	};

	/**
	 * @brief client session
	 */
	class ClientSession {
	private:
		int id;
		OS::Socket * socket;
		int bufferSize;
		int maxBufferSize;
		
	public:
		ClientSession(OS::Socket * socket, int maxBufferSize);
		virtual ~ClientSession();

		int getId();
		OS::Socket * getSocket();
		void setBufferSize(int bufferSize);
		int getBufferSize();
		int getMaxBufferSize();

		bool isClosed();
		void close();

		bool operator==(const ClientSession &other) const;
	};


	/**
	 * @brief on connect listener
	 */
	class OnConnectListener {
	public:
		OnConnectListener() {}
		virtual ~OnConnectListener() {}

		virtual void onClientConnect(MultiConn & server, ClientSession & client) = 0;
	};

	/**
	 * @brief on receive listener
	 */
	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onClientReceive(MultiConn & server, ClientSession & client, Packet & packet) = 0;
	};

	/**
	 * @brief on disconnect listener
	 */
	class OnDisconnectListener {
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onClientDisconnect(MultiConn & server, ClientSession & client) = 0;
	};

	/**
	 * @brief multi conn protocol
	 */
	class MultiConnProtocol : public OnConnectListener,
							  public OnReceiveListener,
							  public OnDisconnectListener {
	public:
        MultiConnProtocol() {}
		virtual ~MultiConnProtocol() {}
		virtual void onClientConnect(MultiConn & server, ClientSession & client) = 0;
		virtual void onClientReceive(MultiConn & server, ClientSession & client, Packet & packet) = 0;
		virtual void onClientDisconnect(MultiConn & server, ClientSession & client) = 0;
	};


	/**
	 * @brief multi conn interface
	 */
	class MultiConn {
	private:
        OnConnectListener * onConnectListener;
		OnReceiveListener * onReceiveListener;
		OnDisconnectListener * onDisconnectListener;
		
	public:
		MultiConn();
		virtual ~MultiConn();

		virtual void start() = 0;
		virtual void poll(unsigned long timeout_milli) = 0;
		virtual void stop() = 0;
		virtual bool isRunning() = 0;

		virtual bool isClientDisconnected(ClientSession & client) = 0;

		virtual void onClientConnect(ClientSession & client);
		virtual void onClientReceive(ClientSession & client, Packet & packet);
		virtual void onClientDisconnect(ClientSession & client);

		void setOnConnectListener(OnConnectListener * onConnectListener);
		void setOnReceiveListener(OnReceiveListener * onReceiveListener);
		void setOnDisconnectListener(OnDisconnectListener * onDisconnectListener);

		void setProtocol(MultiConnProtocol * protocol);
	};

}

#endif
