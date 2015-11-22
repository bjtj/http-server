#ifndef __FILE_TRANSFER_HPP__
#define __FILE_TRANSFER_HPP__

#include <string>

#include <liboslayer/FileReaderWriter.hpp>

#include "Connection.hpp"
#include "Packet.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

namespace HTTP {

	/**
	 * @brief FileTransfer
	 */
	class FileTransfer : public DataTransfer {
	private:

		UTIL::FileReader * reader;
		UTIL::FileWriter * writer;

		LargeReadCounter readCounter;

	public:

		FileTransfer(UTIL::FileReader * reader, unsigned long long size);
		FileTransfer(UTIL::FileWriter * writer, unsigned long long size);
		virtual ~FileTransfer();

        virtual void reset();
		virtual void recv(Packet & packet);
		virtual void send(Connection & connection);

		virtual std::string getString();
	};
}

#endif