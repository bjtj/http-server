#include "ChunkedTransfer.hpp"

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	/**
	 * @brief ChunkedTransfer
	 */
	
	ChunkedTransfer::ChunkedTransfer(AutoRef<DataSource> source) : DataTransfer(source) {
		trailingCounter.setContentSize(2);
    }

	ChunkedTransfer::ChunkedTransfer(AutoRef<DataSink> sink) : DataTransfer(sink) {
		trailingCounter.setContentSize(2);
    }

    ChunkedTransfer::~ChunkedTransfer() {
    }
	
	void ChunkedTransfer::recv(UTIL::AutoRef<Connection> connection) {

		if (sink().nil()) {
			throw Exception("sink required");
		}

		Packet & packet = connection->read();
        char * p = packet.getData();
		size_t packetLength = packet.getLength();
		
        for (size_t i = 0; i < packetLength; i++, p++) {

            char ch = *p;
            
            if (!readerBuffer.hasSizeRecognized()) {
                readerBuffer.readChunkSize(ch);
            } else {
				if (!readerBuffer.completeData()) {
					readerBuffer.write(p, 1);

					if (readerBuffer.completeData()) {
						trailingCounter.resetPosition();
					}
				} else if (!trailingCounter.completed()) {
					trailingCounter.read(1);
				}

				if (trailingCounter.completed()) {

					string chunk(readerBuffer.getChunkData(), readerBuffer.getChunkSize());
					
					sink()->write(readerBuffer.getChunkData(), readerBuffer.getChunkSize());

					if (readerBuffer.getChunkSize() == 0) {
						complete();
					}

					trailingCounter.resetPosition();
					readerBuffer.clear();
				}
            }
        }
    }

	void ChunkedTransfer::send(UTIL::AutoRef<Connection> connection) {
		// TODO: need chunk generator

		if (source().nil()) {
			throw Exception("source required");
		}
	}

	unsigned long long ChunkedTransfer::size() {
		throw Exception("not available");
	}
}
