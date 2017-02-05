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
        virtual UTIL::AutoRef<OS::ServerSocket> makeServerSocket(int port) = 0;
    };

	/**
	 * @brief default server socket maker
	 */
    class DefaultServerSocketMaker : public ServerSocketMaker {
    private:
    public:
        DefaultServerSocketMaker() {}
        virtual ~DefaultServerSocketMaker() {}
        virtual UTIL::AutoRef<OS::ServerSocket> makeServerSocket(int port) {
            return UTIL::AutoRef<OS::ServerSocket>(new OS::ServerSocket(port));
        }
    };
    
	/**
	 * @brief ConnectionMaker
	 */
    
    class ConnectionMaker {
    private:
    public:
        
        ConnectionMaker() {}
        virtual ~ConnectionMaker() {}
        
        virtual Connection * makeConnection() = 0;
    };
    
	/**
	 * @brief CommunicationMaker
	 */

    class CommunicationMaker {
    private:
    public:
        
        CommunicationMaker() {}
        virtual ~CommunicationMaker() {}
        
        virtual UTIL::AutoRef<Communication> makeCommunication() = 0;
    };

	/**
	 *
	 */
	class OnMaxCapacity {
	public:
		OnMaxCapacity() {}
		virtual ~OnMaxCapacity() {}
		virtual void onMaxCapacity(ConnectionManager & cm, UTIL::AutoRef<Connection> connection) = 0;
	};

    
	/**
	 * @brief ConnectionManager
	 */

    class ConnectionManager : public UTIL::Observer {
    private:
		UTIL::AutoRef<ServerSocketMaker> serverSocketMaker;
		UTIL::AutoRef<OS::ServerSocket> serverSocket;
        OS::Selector selector;
        std::map<int, UTIL::AutoRef<Connection> > connections;
        OS::Semaphore connectionsLock;
		UTIL::AutoRef<CommunicationMaker> communicationMaker;
		ConnectionThreadPool threadPool;
		UTIL::AutoRef<OnMaxCapacity> onMaxCapacity;
		unsigned long recvTimeout;
        
    public:
        ConnectionManager(UTIL::AutoRef<CommunicationMaker> communicationMaker,
						  size_t threadCount);
        ConnectionManager(UTIL::AutoRef<CommunicationMaker> communicationMaker,
						  size_t threadCount,
						  UTIL::AutoRef<ServerSocketMaker> serverSocketMaker);
        virtual ~ConnectionManager();
		void start(int port);
        void start(int port, int backlog);
		void stop();
        void poll(unsigned long timeout);
        void clearConnections();
		void onConnect(UTIL::AutoRef<OS::Socket> client);
		UTIL::AutoRef<Connection> makeConnection(UTIL::AutoRef<OS::Socket> client);
        void onDisconnect(UTIL::AutoRef<Connection> connection);
		void registerConnection(UTIL::AutoRef<Connection> connection);
		void unregisterConnection(UTIL::AutoRef<Connection> connection);
		void removeCompletedConnections();
        void stopAllThreads();
		void startCommunication(UTIL::AutoRef<Communication> communication, UTIL::AutoRef<Connection> connection);
		size_t getConnectionCount();
		virtual void onUpdate(UTIL::Observable * target);
		void handleMaxCapacity(UTIL::AutoRef<Connection> connection);
		void setOnMaxCapacity(UTIL::AutoRef<OnMaxCapacity> onMaxCapacity);
		void setRecvTimeout(unsigned long recvTimeout);
		unsigned long getRecvTimeout();
		size_t available();
		size_t working();
		size_t capacity();
        std::vector<UTIL::AutoRef<Connection> > getConnectionList();
    };
}

#endif
