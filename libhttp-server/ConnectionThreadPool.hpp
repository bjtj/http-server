#ifndef __CONNECTION_THREAD_POOL_HPP__
#define __CONNECTION_THREAD_POOL_HPP__

#include <liboslayer/AutoRef.hpp>
#include <liboslayer/ThreadPool.hpp>

#include "Connection.hpp"
#include "Communication.hpp"

namespace http {

	/**
	 * @brief ConnectionTask
	 */
	class ConnectionTask : public osl::Task {
	private:
		osl::AutoRef<Connection> connection;
		osl::AutoRef<Communication> communication;
        osl::Selector selector;

	public:
		ConnectionTask(osl::AutoRef<Connection> connection,
					   osl::AutoRef<Communication> communication);
		virtual ~ConnectionTask();
		void setConnection(osl::AutoRef<Connection> connection,
						   osl::AutoRef<Communication> communication);
		osl::AutoRef<Connection> getConnection();
		virtual void onTask();
		void connectionTask();
		void testReceiveTimeout();
		bool doReceive();
		bool doWrite();
		bool closing();
	};

	/**
	 * @brief ConnectionThreadPool
	 */
	class ConnectionThreadPool : public osl::ThreadPool {
	private:
	public:
		ConnectionThreadPool(size_t maxThreads);
		virtual ~ConnectionThreadPool();
		void createConnection(osl::AutoRef<Communication> communication,
							  osl::AutoRef<Connection> connection);
	};
}

#endif
