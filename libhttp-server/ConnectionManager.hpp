#ifndef __CONNECTION_MANAGER_HPP__
#define __CONNECTION_MANAGER_HPP__

#include <liboslayer/os.hpp>
#include "Connection.hpp"
#include "Communication.hpp"

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
	 * @brief ConnectionThread
	 */

    class ConnectionThread : public OS::Thread {
    private:
        Connection & connection;
        Communication & communication;
        OS::Selector selector;
        
    public:
        ConnectionThread(Connection & connection, Communication & communication);
        virtual ~ConnectionThread();
        virtual void run();
    };
    

	/**
	 * @brief ConnectionManager
	 */

    class ConnectionManager {
    private:
        OS::ServerSocket * serverSocket;
        OS::Selector selector;
        std::map<int, Connection*> connectionTable;
        std::vector<OS::Thread*> threads;
        OS::Semaphore connectionsLock;
        CommunicationMaker & communicationMaker;
        
    public:
        ConnectionManager(CommunicationMaker & communicationMaker);
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
