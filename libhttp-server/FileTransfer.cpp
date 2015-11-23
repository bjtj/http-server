#include "FileTransfer.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;


	FileTransfer::FileTransfer(FileReader * reader, unsigned long long size) : reader(reader), writer(NULL), readCounter(size) {
	}

	FileTransfer::FileTransfer(FileWriter * writer, unsigned long long size) : reader(NULL), writer(writer), readCounter(size) {
	}

	FileTransfer::~FileTransfer() {
		if (reader) {
			delete reader;
		}
		if (writer) {
			delete writer;
		}
	}

    void FileTransfer::reset() {
        readCounter.resetPosition();
        // TODO: file reader / writer reset position
    }
	void FileTransfer::recv(Packet & packet) {

		readCounter.read(packet.getLength());

		if (writer) {
			writer->write(packet.getData(), packet.getLength());
		}
        
		if (readCounter.complete()) {
            setCompleted();
        }
	}
	void FileTransfer::send(Connection & connection) {

		if (!reader) {
			throw IOException("FileReader required", -1, 0);
		}
		
		if (reader) {
			char buffer[4096] = {0,};
			size_t len = reader->read(buffer, sizeof(buffer));
			connection.send(buffer, len);
			readCounter.read(len);
		}
        
		if (readCounter.complete()) {
            setCompleted();
        }
	}

	unsigned long long FileTransfer::getSize() {
		return readCounter.getContentSize();
	}

	string FileTransfer::getString() {
		return "";
	}
}