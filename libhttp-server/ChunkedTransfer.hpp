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

		std::string stringBuffer;
		ChunkedReaderBuffer readerBuffer;
		ReadCounter trailingCounter;

	public:

		ChunkedTransfer();
		virtual ~ChunkedTransfer();
        virtual void reset();
		virtual void recv(Packet & packet);
		virtual void send(Connection & connection);
		virtual unsigned long long getSize();

		virtual std::string getString();
	};

}

#endif