#ifndef __CONNECTION_THREAD_POOL_HPP__
#define __CONNECTION_THREAD_POOL_HPP__

#include <liboslayer/AutoRef.hpp>
#include <liboslayer/ThreadPool.hpp>

#include "Connection.hpp"
#include "Communication.hpp"

namespace HTTP {

	/**
	 * @brief ConnectionTask
	 */
	class ConnectionTask : public UTIL::Task {
	private:
		OS::AutoRef<Connection> connection;
		OS::AutoRef<Communication> communication;
        OS::Selector selector;

	public:
		ConnectionTask(OS::AutoRef<Connection> connection, OS::AutoRef<Communication> communication);
		virtual ~ConnectionTask();
		void setConnection(OS::AutoRef<Connection> connection, OS::AutoRef<Communication> communication);
		OS::AutoRef<Connection> getConnection();
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
	class ConnectionThreadPool : public UTIL::ThreadPool {
	private:
	public:
		ConnectionThreadPool(size_t maxThreads);
		virtual ~ConnectionThreadPool();
		void createConnection(OS::AutoRef<Communication> communication, OS::AutoRef<Connection> connection);
	};
}

#endif
