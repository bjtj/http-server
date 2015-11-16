#ifndef __MULTI_CONN_HPP__
#define __MULTI_CONN_HPP__

#include <vector>
#include <map>
#include <liboslayer/os.hpp>
#include "Packet.hpp"

namespace HTTP {

	class MultiConn;
	class MultiConnMultiplexServer;
	class MultiConnThreadedServer;


	/**
	 * @brief client session
	 */
	class Connection {
	private:
		int id;
		OS::Socket * socket;
		int bufferSize;
		int maxBufferSize;
		
	public:
		Connection(OS::Socket * socket, int maxBufferSize);
		virtual ~Connection();

		int getId();
		OS::Socket * getSocket();
		void setBufferSize(int bufferSize);
		int getBufferSize();
		int getMaxBufferSize();

		bool isClosed();
		void close();

		bool operator==(const Connection &other) const;
	};


	/**
	 * @brief on connect listener
	 */
	class OnConnectListener {
	public:
		OnConnectListener() {}
		virtual ~OnConnectListener() {}

		virtual void onClientConnect(MultiConn & server, Connection & connection) = 0;
	};

	/**
	 * @brief on receive listener
	 */
	class OnReceiveListener {
	public:
		OnReceiveListener() {}
		virtual ~OnReceiveListener() {}

		virtual void onClientReceive(MultiConn & server, Connection & connection, Packet & packet) = 0;
	};
    
    /**
     * @brief on writeable listener
     */
    class OnWriteableListener {
    private:
    public:
        OnWriteableListener() {}
        virtual ~OnWriteableListener() {}
        
        virtual void onClientWriteable(MultiConn & server, Connection & connection) = 0;
    };

	/**
	 * @brief on disconnect listener
	 */
	class OnDisconnectListener {
	public:
		OnDisconnectListener() {}
		virtual ~OnDisconnectListener() {}

		virtual void onClientDisconnect(MultiConn & server, Connection & connection) = 0;
	};

	/**
	 * @brief multi conn protocol
	 */
	class MultiConnProtocol : public OnConnectListener, public OnReceiveListener, public OnWriteableListener, public OnDisconnectListener {
	public:
        MultiConnProtocol() {}
		virtual ~MultiConnProtocol() {}
		virtual void onClientConnect(MultiConn & server, Connection & connection) = 0;
		virtual void onClientReceive(MultiConn & server, Connection & connection, Packet & packet) = 0;
        virtual void onClientWriteable(MultiConn & server, Connection & connection) = 0;
		virtual void onClientDisconnect(MultiConn & server, Connection & connection) = 0;
	};


	/**
	 * @brief multi conn interface
	 */
	class MultiConn {
	private:
        OnConnectListener * onConnectListener;
		OnReceiveListener * onReceiveListener;
        OnWriteableListener * onWriteableListener;
		OnDisconnectListener * onDisconnectListener;
		
	public:
		MultiConn();
		virtual ~MultiConn();

		virtual void start() = 0;
		virtual void poll(unsigned long timeout_milli) = 0;
		virtual void stop() = 0;
		virtual bool isRunning() = 0;

		virtual void onClientConnect(Connection & connection);
		virtual void onClientReceive(Connection & connection, Packet & packet);
        virtual void onClientWriteable(Connection & connection);
		virtual void onClientDisconnect(Connection & connection);

		void setOnConnectListener(OnConnectListener * onConnectListener);
		void setOnReceiveListener(OnReceiveListener * onReceiveListener);
        void setOnWriteableListener(OnWriteableListener * onWriteableListener);
		void setOnDisconnectListener(OnDisconnectListener * onDisconnectListener);

		void setProtocol(MultiConnProtocol * protocol);
	};

}

#endif
