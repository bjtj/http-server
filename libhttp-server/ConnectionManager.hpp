#ifndef __CONNECTION_MANAGER_HPP__
#define __CONNECTION_MANAGER_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include "Connection.hpp"
#include "Communication.hpp"
#include "ConnectionThreadPool.hpp"

#include <map>
#include <vector>

namespace HTTP {

	class ConnectionManager;

	/**
	 * @brief server socket maker
	 */
    class ServerSocketMaker {
    private:
    public:
        ServerSocketMaker() {}
        virtual ~ServerSocketMaker() {}
        virtual OS::AutoRef<OS::ServerSocket> makeServerSocket(int port) = 0;
    };
    
	/**
	 * @brief CommunicationMaker
	 */

    class CommunicationMaker {
    private:
    public:
        
        CommunicationMaker() {}
        virtual ~CommunicationMaker() {}
        
        virtual OS::AutoRef<Communication> makeCommunication() = 0;
    };

	/**
	 *
	 */
	class OnMaxCapacity {
	public:
		OnMaxCapacity() {}
		virtual ~OnMaxCapacity() {}
		virtual void onMaxCapacity(ConnectionManager & cm, OS::AutoRef<Connection> connection) = 0;
	};


	/**
	 * connection config
	 */
	class ConnectionConfig
	{
	private:
		OS::AutoRef<CommunicationMaker> _communicationMaker;
		OS::AutoRef<ServerSocketMaker> _serverSocketMaker;
		size_t _threadCount;
		
	public:
		ConnectionConfig(OS::AutoRef<CommunicationMaker> communicationMaker,
						 size_t threadCount);
		ConnectionConfig(OS::AutoRef<CommunicationMaker> communicationMaker,
						 OS::AutoRef<ServerSocketMaker> serverSocketMaker,
						 size_t threadCount);
		virtual ~ConnectionConfig();
		OS::AutoRef<CommunicationMaker> & communicationMaker();
		OS::AutoRef<ServerSocketMaker> & serverSocketMaker();
		size_t & threadCount();
		OS::AutoRef<CommunicationMaker> communicationMaker() const;
		OS::AutoRef<ServerSocketMaker> serverSocketMaker() const;
		size_t threadCount() const;
	};


    
	/**
	 * @brief ConnectionManager
	 */
    class ConnectionManager : public UTIL::Observer {
    private:
		ConnectionConfig _config;
		OS::AutoRef<OS::ServerSocket> serverSocket;
        OS::Selector selector;
        std::map<int, OS::AutoRef<Connection> > connections;
        OS::Semaphore connectionsLock;
		ConnectionThreadPool threadPool;
		OS::AutoRef<OnMaxCapacity> onMaxCapacity;
		unsigned long recvTimeout;
        
    public:
		ConnectionManager(const ConnectionConfig & config);
        virtual ~ConnectionManager();
		void start(int port);
        void start(int port, int backlog);
		void stop();
        void poll(unsigned long timeout);
        void clearConnections();
		void onConnect(OS::AutoRef<OS::Socket> client);
		OS::AutoRef<Connection> makeConnection(OS::AutoRef<OS::Socket> client);
        void onDisconnect(OS::AutoRef<Connection> connection);
		void registerConnection(OS::AutoRef<Connection> connection);
		void unregisterConnection(OS::AutoRef<Connection> connection);
		void removeCompletedConnections();
        void stopAllThreads();
		void startCommunication(OS::AutoRef<Communication> communication,
								OS::AutoRef<Connection> connection);
		size_t getConnectionCount();
		virtual void onUpdate(UTIL::Observable * target);
		void handleMaxCapacity(OS::AutoRef<Connection> connection);
		void setOnMaxCapacity(OS::AutoRef<OnMaxCapacity> onMaxCapacity);
		void setRecvTimeout(unsigned long recvTimeout);
		unsigned long getRecvTimeout();
		size_t available();
		size_t working();
		size_t capacity();
        std::vector<OS::AutoRef<Connection> > getConnectionList();
    };
}

#endif
