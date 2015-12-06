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
        XOS::ServerSocket * serverSocket;
        OS::Selector selector;
        std::map<int, Connection*> connectionTable;
        OS::Semaphore connectionsLock;
        CommunicationMaker & communicationMaker;

		ConnectionThreadPool threadPool;
        
    public:
        ConnectionManager(CommunicationMaker & communicationMaker);
        virtual ~ConnectionManager();
        virtual Connection * makeConnection(XOS::Socket & client);
        virtual void removeConnection(Connection * connection);
        void onConnect(XOS::Socket & client);
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
