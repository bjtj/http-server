#ifndef __CONNECTION_MANAGER_HPP__
#define __CONNECTION_MANAGER_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include "Connection.hpp"
#include "Communication.hpp"
#include "ConnectionThreadPool.hpp"

#include <map>
#include <vector>

namespace http {

	class ConnectionManager;

	/**
	 * @brief server socket maker
	 */
    class ServerSocketMaker {
    private:
    public:
        ServerSocketMaker() {}
        virtual ~ServerSocketMaker() {}
        virtual osl::AutoRef<osl::ServerSocket> makeServerSocket(int port) = 0;
    };
    
	/**
	 * @brief CommunicationMaker
	 */

    class CommunicationMaker {
    private:
    public:
        
        CommunicationMaker() {}
        virtual ~CommunicationMaker() {}
        
        virtual osl::AutoRef<Communication> makeCommunication() = 0;
    };

	/**
	 *
	 */
	class OnMaxCapacity {
	public:
		OnMaxCapacity() {}
		virtual ~OnMaxCapacity() {}
		virtual void onMaxCapacity(ConnectionManager & cm, osl::AutoRef<Connection> connection) = 0;
	};


	/**
	 * connection config
	 */
	class ConnectionConfig
	{
	private:
		osl::AutoRef<CommunicationMaker> _communicationMaker;
		osl::AutoRef<ServerSocketMaker> _serverSocketMaker;
		size_t _threadCount;
		
	public:
		ConnectionConfig(osl::AutoRef<CommunicationMaker> communicationMaker,
						 size_t threadCount);
		ConnectionConfig(osl::AutoRef<CommunicationMaker> communicationMaker,
						 osl::AutoRef<ServerSocketMaker> serverSocketMaker,
						 size_t threadCount);
		virtual ~ConnectionConfig();
		osl::AutoRef<CommunicationMaker> & communicationMaker();
		osl::AutoRef<ServerSocketMaker> & serverSocketMaker();
		size_t & threadCount();
		osl::AutoRef<CommunicationMaker> communicationMaker() const;
		osl::AutoRef<ServerSocketMaker> serverSocketMaker() const;
		size_t threadCount() const;
	};


    
	/**
	 * @brief ConnectionManager
	 */
    class ConnectionManager : public osl::Observer {
    private:
		ConnectionConfig _config;
		osl::AutoRef<osl::ServerSocket> serverSocket;
        osl::Selector selector;
        std::map<int, osl::AutoRef<Connection> > connections;
        osl::Semaphore connectionsLock;
		ConnectionThreadPool threadPool;
		osl::AutoRef<OnMaxCapacity> onMaxCapacity;
		unsigned long recvTimeout;
        
    public:
		ConnectionManager(const ConnectionConfig & config);
        virtual ~ConnectionManager();
		void start(int port);
        void start(int port, int backlog);
		void stop();
		osl::InetAddress getServerAddress();
        void poll(unsigned long timeout);
        void clearConnections();
		void onConnect(osl::AutoRef<osl::Socket> client);
		osl::AutoRef<Connection> makeConnection(osl::AutoRef<osl::Socket> client);
        void onDisconnect(osl::AutoRef<Connection> connection);
		void registerConnection(osl::AutoRef<Connection> connection);
		void unregisterConnection(osl::AutoRef<Connection> connection);
		void removeCompletedConnections();
        void stopAllThreads();
		void startCommunication(osl::AutoRef<Communication> communication,
								osl::AutoRef<Connection> connection);
		size_t getConnectionCount();
		virtual void onUpdate(osl::Observable * target);
		void handleMaxCapacity(osl::AutoRef<Connection> connection);
		void setOnMaxCapacity(osl::AutoRef<OnMaxCapacity> onMaxCapacity);
		void setRecvTimeout(unsigned long recvTimeout);
		unsigned long getRecvTimeout();
		size_t available();
		size_t working();
		size_t capacity();
        std::vector<osl::AutoRef<Connection> > getConnectionList();
    };
}

#endif
