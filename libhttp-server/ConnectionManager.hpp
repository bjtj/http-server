#ifndef __CONNECTION_MANAGER_HPP__
#define __CONNECTION_MANAGER_HPP__

#include <liboslayer/os.hpp>
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
        
        virtual Communication * makeCommunication() = 0;
    };
    
	/**
	 * @brief ConnectionManager
	 */

    class ConnectionManager {
    private:
        ServerSocketMaker * serverSocketMaker;
        OS::ServerSocket * serverSocket;
        OS::Selector selector;
        std::map<int, Connection*> connectionTable;
        OS::Semaphore connectionsLock;
        CommunicationMaker & communicationMaker;

		ConnectionThreadPool threadPool;
        
    public:
        ConnectionManager(CommunicationMaker & communicationMaker, size_t threadCount);
        ConnectionManager(CommunicationMaker & communicationMaker, size_t threadCount, ServerSocketMaker * serverSocketMaker);
        virtual ~ConnectionManager();
        virtual Connection * makeConnection(OS::Socket & client);
        virtual void removeConnection(Connection * connection);
        void onConnect(OS::Socket & client);
        void onDisconnect(Connection * connection);
        void clearConnections();
        void start(int port);
        void poll(unsigned long timeout);
		void removeCompletedConnections();
        void removeCompletedThreads();
        void stop();
        void stopAllThreads();

		void startCommunication(Communication * communication, Connection * connection);
    };
}

#endif
