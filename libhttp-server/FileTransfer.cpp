#include "FileTransfer.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;


	FileTransfer::FileTransfer(AutoRef<FileReader> reader, unsigned long long size) : reader(reader), readCounter(size) {
	}

	FileTransfer::FileTransfer(AutoRef<FileWriter> writer, unsigned long long size) : writer(writer), readCounter(size) {
	}

	FileTransfer::~FileTransfer() {
	}

    void FileTransfer::reset() {
        readCounter.resetPosition();
        // TODO: file reader / writer reset position
    }
	void FileTransfer::recv(Connection & connection) {

		Packet & packet = connection.read();
		readCounter.read(packet.getLength());

		if (!writer.nil()) {
			writer->write(packet.getData(), packet.getLength());
		}
        
		if (readCounter.complete()) {
            setCompleted();
        }
	}
	void FileTransfer::send(Connection & connection) {

		if (reader.nil()) {
			throw IOException("FileReader required", -1, 0);
		}
		
		char buffer[4096] = {0,};
		size_t len = reader->read(buffer, sizeof(buffer));
		connection.send(buffer, len);
		readCounter.read(len);

		if (readCounter.complete()) {
            setCompleted();
        }
	}

	unsigned long long FileTransfer::getSize() {
		return readCounter.getContentSize();
	}

	string FileTransfer::getString() {
		throw Exception("FileTrasnfer not support getString()", -1, 0);
	}
}
