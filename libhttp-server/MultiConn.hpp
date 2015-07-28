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

		virtual void onConnect(MultiConn & server, OS::Socket & client) = 0;
	};

	/**
	 * @brief on receive listener
	 */
	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onReceive(MultiConn & server, OS::Socket & client, Packet & packet) = 0;
	};

	/**
	 * @brief on disconnect listener
	 */
	class OnDisconnectListener {
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onDisconnect(MultiConn & server, OS::Socket & client) = 0;
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
		virtual void onConnect(MultiConn & server, OS::Socket & client) = 0;
		virtual void onReceive(MultiConn & server, OS::Socket & client, Packet & packet) = 0;
		virtual void onDisconnect(MultiConn & server, OS::Socket & client) = 0;
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

		virtual bool isDisconnected(OS::Socket & client) = 0;
		virtual void disconnect(OS::Socket & client) = 0;

		virtual void onConnect(OS::Socket & client);
		virtual void onReceive(OS::Socket & client, Packet & packet);
		virtual void onDisconnect(OS::Socket & client);

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
		std::map<int, OS::Socket*> clients;
		OS::ServerSocket * server;

	public:
		MultiConnMultiplexServer(int port);
		virtual ~MultiConnMultiplexServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();

		virtual bool isDisconnected(OS::Socket & client);
		virtual void disconnect(OS::Socket & client);

		virtual void onConnect(OS::Socket & client);
		virtual void onReceive(OS::Socket & client, Packet & packet);
		virtual void onDisconnect(OS::Socket & client);
	};

	/**
	 * @brief client thread
	 */
	class ClientThread : public OS::Thread {
	private:
		MultiConnThreadedServer & server;
		OS::Socket & socket;
	public:
		ClientThread(MultiConnThreadedServer & server, OS::Socket & socket);
		virtual ~ClientThread();
		virtual void run();
		OS::Socket & getSocket();
	};
	
	/**
	 * @brief multi conn threaded server
	 */
	class MultiConnThreadedServer : public MultiConn {
	private:
		int port;
		OS::Selector selector;
        std::map<int, ClientThread *> clients;
		OS::ServerSocket * server;
	public:
		MultiConnThreadedServer(int port);
		virtual ~MultiConnThreadedServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		virtual void stop();
		virtual bool isRunning();

		void releaseInvalidThreads();

		virtual bool isDisconnected(OS::Socket & client);
		virtual void disconnect(OS::Socket & client);

		virtual void onConnect(OS::Socket & client);
		virtual void onReceive(OS::Socket & client, Packet & packet);
		virtual void onDisconnect(OS::Socket & client);
	};

}

#endif
