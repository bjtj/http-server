#ifndef __CONNECTION_THREAD_POOL_HPP__
#define __CONNECTION_THREAD_POOL_HPP__

#include <liboslayer/ThreadPool.hpp>

#include "Connection.hpp"
#include "Communication.hpp"

namespace HTTP {

	/**
	 * @brief ConnectionThread
	 */
	class ConnectionThread : public UTIL::FlaggableThread {
	private:

		Connection * connection;
        Communication * communication;
        OS::Selector selector;

	public:

		ConnectionThread();
		virtual ~ConnectionThread();

		void setConnection(Connection * connection, Communication * communication);
		virtual void run();
		void connectionTask();
	};


	/**
	 * @brief ConnectionThreadPool
	 */
	class ConnectionThreadPool : public UTIL::ThreadPool {
	private:
	public:
		ConnectionThreadPool(size_t maxThreads);
		virtual ~ConnectionThreadPool();

		void createConnection(Communication * communication, Connection * connection);

	};

}

#endif