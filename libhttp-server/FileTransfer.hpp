#ifndef __FILE_TRANSFER_HPP__
#define __FILE_TRANSFER_HPP__

#include <string>

#include <liboslayer/AutoRef.hpp>
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

		UTIL::AutoRef<UTIL::FileReader> reader;
		UTIL::AutoRef<UTIL::FileWriter> writer;

		LargeReadCounter readCounter;

	public:

		FileTransfer(UTIL::AutoRef<UTIL::FileReader> reader, unsigned long long size);
		FileTransfer(UTIL::AutoRef<UTIL::FileWriter> writer, unsigned long long size);
		virtual ~FileTransfer();

        virtual void reset();
		virtual void recv(Connection & connection);
		virtual void send(Connection & connection);
		virtual unsigned long long getSize();

		virtual std::string getString();
	};
}

#endif
