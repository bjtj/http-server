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
		ClientSession & client;
		
	public:
		ClientHandlerThread(MultiConnThreadedServer & server, ClientSession & client);
		virtual ~ClientHandlerThread();
		virtual void run();
		ClientSession & getClient();
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
		virtual void onClientConnect(ClientSession & client);
		virtual void onClientReceive(ClientSession & client, Packet & packet);
		virtual void onClientDisconnect(ClientSession & client);

		virtual void stop();
		virtual bool isRunning();
	};

}

#endif