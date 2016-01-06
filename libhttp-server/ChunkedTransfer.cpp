#include "ChunkedTransfer.hpp"

#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();

	/**
	 * @brief ChunkedTransfer
	 */
	
	ChunkedTransfer::ChunkedTransfer() {
		trailingCounter.setContentSize(2);
    }

    ChunkedTransfer::~ChunkedTransfer() {
    }

    void ChunkedTransfer::reset() {
        stringBuffer.clear();
        readerBuffer.clear();
        trailingCounter.resetPosition();
    }
	void ChunkedTransfer::recv(Connection & connection) {

		Packet & packet = connection.read();
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
				} else if (!trailingCounter.complete()) {
					trailingCounter.read(1);
				}

				if (trailingCounter.complete()) {

					string chunk(readerBuffer.getChunkData(), readerBuffer.getChunkSize());
					// logger.logv(" > " + chunk);

					stringBuffer.append(readerBuffer.getChunkData(), readerBuffer.getChunkSize());

					if (readerBuffer.getChunkSize() == 0) {
						setCompleted();
					}

					trailingCounter.resetPosition();
					readerBuffer.clear();
				}
            }
        }
    }

	void ChunkedTransfer::send(Connection & connection) {
		// TODO: need chunk generator
	}

	unsigned long long ChunkedTransfer::getSize() {
		throw Exception("unpredictable", -1, 0);
	}

	string ChunkedTransfer::getString() {
		return stringBuffer;
	}
}