#ifndef __CHUNKED_TRANSFER_HPP__
#define __CHUNKED_TRANSFER_HPP__

#include "Connection.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

namespace http {

	/**
	 * @brief ChunkedTransfer
	 */
	class ChunkedTransfer : public DataTransfer {
	private:

		ChunkedReaderBuffer readerBuffer;
		ReadCounter trailingCounter;

	public:

		ChunkedTransfer(osl::AutoRef<DataSource> source);
		ChunkedTransfer(osl::AutoRef<DataSink> sink);
		virtual ~ChunkedTransfer();
		virtual void recv(osl::AutoRef<Connection> connection);
		virtual void send(osl::AutoRef<Connection> connection);
		virtual unsigned long long size();
	};

}

#endif
