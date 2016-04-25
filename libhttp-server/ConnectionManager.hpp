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

	/**
	 * @brief server socket maker
	 */
    class ServerSocketMaker {
    private:
    public:
        ServerSocketMaker() {}
        virtual ~ServerSocketMaker() {}
        virtual OS::ServerSocket * makeServerSocket(int port) = 0;
		virtual void releaseSocket(OS::ServerSocket * sock) = 0;
    };

	/**
	 * @brief default server socket maker
	 */
    class DefaultServerSocketMaker : public ServerSocketMaker {
    private:
    public:
        DefaultServerSocketMaker() {}
        virtual ~DefaultServerSocketMaker() {}
        virtual OS::ServerSocket * makeServerSocket(int port) {
            return new OS::ServerSocket(port);
        }
		virtual void releaseSocket(OS::ServerSocket * sock) {
			delete sock;
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
	 * @brief ConnectionManager
	 */

    class ConnectionManager : public UTIL::Observer {
    private:
		UTIL::AutoRef<ServerSocketMaker> serverSocketMaker;
        OS::ServerSocket * serverSocket;
        OS::Selector selector;
        std::map<int, UTIL::AutoRef<Connection> > connectionTable;
        OS::Semaphore connectionsLock;
		UTIL::AutoRef<CommunicationMaker> communicationMaker;
		ConnectionThreadPool threadPool;
        
    public:
        ConnectionManager(UTIL::AutoRef<CommunicationMaker> communicationMaker, size_t threadCount);
        ConnectionManager(UTIL::AutoRef<CommunicationMaker> communicationMaker, size_t threadCount, UTIL::AutoRef<ServerSocketMaker> serverSocketMaker);
        virtual ~ConnectionManager();
        virtual UTIL::AutoRef<Connection> makeConnection(UTIL::AutoRef<OS::Socket> client);
        void onConnect(UTIL::AutoRef<OS::Socket> client);
        void onDisconnect(UTIL::AutoRef<Connection> connection);
        void clearConnections();
        void start(int port);
        void poll(unsigned long timeout);
		void removeCompletedConnections();
        void stop();
        void stopAllThreads();
		void startCommunication(UTIL::AutoRef<Communication> communication, UTIL::AutoRef<Connection> connection);
		size_t getConnectionCount();
		virtual void update(UTIL::Observable * target);
    };
}

#endif
