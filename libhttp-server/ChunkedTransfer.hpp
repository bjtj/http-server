#ifndef __CHUNKED_TRANSFER_HPP__
#define __CHUNKED_TRANSFER_HPP__

#include "Connection.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

namespace HTTP {

	/**
	 * @brief ChunkedTransfer
	 */
	class ChunkedTransfer : public DataTransfer {
	private:

		ChunkedReaderBuffer readerBuffer;
		ReadCounter trailingCounter;

	public:

		ChunkedTransfer(UTIL::AutoRef<DataSource> source);
		ChunkedTransfer(UTIL::AutoRef<DataSink> sink);
		virtual ~ChunkedTransfer();
		virtual void recv(UTIL::AutoRef<Connection> connection);
		virtual void send(UTIL::AutoRef<Connection> connection);
		virtual unsigned long long size();
	};

}

#endif
