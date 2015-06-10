#ifndef __MULTI_CONN_HPP__
#define __MULTI_CONN_HPP__

#include <vector>
#include "os.hpp"

namespace HTTP {

	class Connection
	{
	public:
		Connection();
		virtual ~Connection();
	};

	class OnAccept
	{
	public:
		OnAccept();
		virtual ~OnAccept();
	};

	class MultiConn {
	private:
		std::vector<Connection> conns;
	public:
		MultiConn();
		virtual ~MultiConn();

		void poll();
	};

	
}

#endif
