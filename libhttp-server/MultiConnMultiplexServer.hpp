#ifndef __MULTI_CONN_MULTIPLEX_SERVER_HPP__
#define __MULTI_CONN_MULTIPLEX_SERVER_HPP__

#include "MultiConn.hpp"

namespace HTTP {
	/**
	 * @brief multi connection server
	 */
	class MultiConnMultiplexServer : public MultiConn {
	private:
		int port;

		OS::Selector selector;
		std::map<int, ClientSession*> clients;
		OS::ServerSocket * server;

	public:
		MultiConnMultiplexServer(int port);
		virtual ~MultiConnMultiplexServer();

		virtual void start();
		virtual void poll(unsigned long timeout_milli);
        virtual void listen();
		virtual void stop();
		virtual bool isRunning();

		virtual bool isClientDisconnected(ClientSession & client);

		virtual void onClientConnect(ClientSession & client);
		virtual void onClientReceive(ClientSession & client, Packet & packet);
		virtual void onClientDisconnect(ClientSession & client);
	};
}

#endif