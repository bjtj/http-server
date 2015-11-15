#ifndef __MULTI_CONN_THREADED_SERVER_HPP__
#define __MULTI_CONN_THREADED_SERVER_HPP__

#include "MultiConn.hpp"
#include <liboslayer/os.hpp>

namespace HTTP {

	/**
	 * @brief client thread
	 */
	class ClientHandlerThread : public OS::Thread {
	private:
		MultiConnThreadedServer & server;
		Connection & connection;
		
	public:
		ClientHandlerThread(MultiConnThreadedServer & server, Connection & connection);
		virtual ~ClientHandlerThread();
		virtual void run();
		Connection & getClient();
		void quit();
	};
	
	/**
	 * @brief multi conn threaded server
	 */
	class MultiConnThreadedServer : public MultiConn {
	private:
		int port;
		OS::Selector selector;
		OS::Semaphore clientThreadsLock;
        std::map<int, ClientHandlerThread *> clientThreads;
		OS::ServerSocket * server;
		
	public:
		MultiConnThreadedServer(int port);
		virtual ~MultiConnThreadedServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
		void releaseInvalidThreads();

        virtual void listen();
		virtual void onClientConnect(Connection & connection);
		virtual void onClientReceive(Connection & connection, Packet & packet);
		virtual void onClientDisconnect(Connection & connection);

		virtual void stop();
		virtual bool isRunning();
	};

}

#endif