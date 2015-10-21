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

		bool operator==(const ClientSession &other) const;
	};


	/**
	 * @brief on connect listener
	 */
	class OnConnectListener {
	public:
		OnConnectListener() {}
		virtual ~OnConnectListener() {}

		virtual void onConnect(MultiConn & server, ClientSession & client) = 0;
	};

	/**
	 * @brief on receive listener
	 */
	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onReceive(MultiConn & server, ClientSession & client, Packet & packet) = 0;
	};

	/**
	 * @brief on disconnect listener
	 */
	class OnDisconnectListener {
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onDisconnect(MultiConn & server, ClientSession & client) = 0;
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
		virtual void onConnect(MultiConn & server, ClientSession & client) = 0;
		virtual void onReceive(MultiConn & server, ClientSession & client, Packet & packet) = 0;
		virtual void onDisconnect(MultiConn & server, ClientSession & client) = 0;
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

		virtual bool isDisconnected(ClientSession & client) = 0;
		virtual void disconnect(ClientSession & client) = 0;

		virtual void onConnect(ClientSession & client);
		virtual void onReceive(ClientSession & client, Packet & packet);
		virtual void onDisconnect(ClientSession & client);

		void setOnConnectListener(OnConnectListener * onConnectListener);
		void setOnReceiveListener(OnReceiveListener * onReceiveListener);
		void setOnDisconnectListener(OnDisconnectListener * onDisconnectListener);

		void setProtocol(MultiConnProtocol * protocol);
	};


	/**
	 * @brief multi connection server
	 */
	class MultiConnMultiplexServer : public MultiConn {
	private:
		int port;

		OS::Selector selector;
		std::map<int, ClientSession*> clients;
		OS::ServerSocket * server;

	public:
		MultiConnMultiplexServer(int port);
		virtual ~MultiConnMultiplexServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();

		virtual bool isDisconnected(ClientSession & client);
		virtual void disconnect(ClientSession & client);

		virtual void onConnect(ClientSession & client);
		virtual void onReceive(ClientSession & client, Packet & packet);
		virtual void onDisconnect(ClientSession & client);
	};

	/**
	 * @brief client thread
	 */
	class ClientHandlerThread : public OS::Thread {
	private:
		MultiConnThreadedServer & server;
		ClientSession & client;
		
	public:
		ClientHandlerThread(MultiConnThreadedServer & server, ClientSession & client);
		virtual ~ClientHandlerThread();
		virtual void run();
		ClientSession & getClient();
		void quit();
	};
	
	/**
	 * @brief multi conn threaded server
	 */
	class MultiConnThreadedServer : public MultiConn {
	private:
		int port;
		OS::Selector selector;
        std::map<int, ClientHandlerThread *> clients;
		OS::ServerSocket * server;
		
	public:
		MultiConnThreadedServer(int port);
		virtual ~MultiConnThreadedServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();

		void releaseInvalidThreads();

		virtual bool isDisconnected(ClientSession & client);
		virtual void disconnect(ClientSession & client);

		virtual void onConnect(ClientSession & client);
		virtual void onReceive(ClientSession & client, Packet & packet);
		virtual void onDisconnect(ClientSession & client);
	};

}

#endif
