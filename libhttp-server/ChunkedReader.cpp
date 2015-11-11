#include "ChunkedReader.hpp"

#include <liboslayer/Text.hpp>
#include <string>

#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();
    
    ChunkedBuffer::ChunkedBuffer() : chunkDataBuffer(NULL) {
        clear();
    }

    ChunkedBuffer::ChunkedBuffer(const ChunkedBuffer & other) {
		chunkDataBuffer = NULL;
        chunkSize = other.chunkSize;
        chunkDataReadPosition = other.chunkDataReadPosition;

		if (chunkSize > 0) {
			chunkDataBuffer = new char[chunkSize];
			memcpy(chunkDataBuffer, other.chunkDataBuffer, chunkSize);
		}
	}

    ChunkedBuffer::~ChunkedBuffer() {
        clear();
    }
    
    void ChunkedBuffer::clear() {
        if (chunkDataBuffer) {
            delete [] chunkDataBuffer;
        }
        chunkDataBuffer = NULL;
        chunkSize = 0;
        chunkDataReadPosition = 0;
    }
    
    void ChunkedBuffer::readChunkData(const char * data, size_t len) {
        size_t remain = remainingDataBuffer();
        size_t writeSize = (len < remain ? len : remain);
        memcpy(chunkDataBuffer + chunkDataReadPosition, data, writeSize);
        chunkDataReadPosition += writeSize;
    }
    
    size_t ChunkedBuffer::remainingDataBuffer() const {
        return (chunkDataReadPosition <= chunkSize ? chunkSize - chunkDataReadPosition : 0);
    }

    bool ChunkedBuffer::remain() const {
		return (remainingDataBuffer() > 0);
	}

    bool ChunkedBuffer::completeData() const {
        return (remainingDataBuffer() == 0);
    }
    
    void ChunkedBuffer::setChunkSize(size_t chunkSize) {
        clear();
        this->chunkSize = chunkSize;
        chunkDataBuffer = new char[chunkSize];
    }
    
    size_t ChunkedBuffer::getChunkSize() const {
        return chunkSize;
    }
    
    const char * ChunkedBuffer::getChunkData() const {
        return chunkDataBuffer;
    }
    
    size_t ChunkedBuffer::getReadSize(size_t bufferSize) const {
        size_t remain = remainingDataBuffer();
        return bufferSize > remain ? remain : bufferSize;
    }
    
    size_t ChunkedBuffer::getCurrentReadPosition() const {
        return chunkDataReadPosition;
    }
    

	/**
	 * @brief Chunked Read Buffer
	 */
	ChunkedReaderBuffer::ChunkedReaderBuffer() {
		clear();
	}
	ChunkedReaderBuffer::~ChunkedReaderBuffer() {
		clear();
	}

	void ChunkedReaderBuffer::clear() {
        chunkSizeBuffer.clear();
        chunkSizeRecognized = false;
        ChunkedBuffer::clear();
	}
	void ChunkedReaderBuffer::readChunkSize(char ch) {
		chunkSizeBuffer.append(1, ch);
		if (Text::endsWith(chunkSizeBuffer, "\r\n")) {
            setChunkSize(Text::toInt(chunkSizeBuffer, 16));
			chunkSizeRecognized = true;
		}
	}
    bool ChunkedReaderBuffer::hasSizeRecognized() const {
		return chunkSizeRecognized;
	}

	/**
	 * @brief consume
	 */
	ConsumeBuffer::ConsumeBuffer(size_t maxSize) : pos(0), maxSize(maxSize) {
	}
	ConsumeBuffer::~ConsumeBuffer() {
	}

	void ConsumeBuffer::clear() {
		pos = 0;
	}
	void ConsumeBuffer::read(size_t len) {
		size_t writeSize = len < remaining() ? len : remaining();
		pos += writeSize;
	}
	size_t ConsumeBuffer::remaining() const {
		return (pos < maxSize ? maxSize - pos : 0);
	}
	bool ConsumeBuffer::complete() const {
		return remaining() == 0;
	}

	void ConsumeBuffer::setMaxSize(size_t maxSize) {
		this->maxSize = maxSize;
	}
    
    size_t ConsumeBuffer::getReadSize(size_t bufferSize) const {
        size_t remain = remaining();
        return bufferSize > remain ? remain : bufferSize;
    }


	/**
	 * @brief Chunked Reader
	 */
	ChunkedReader::ChunkedReader(OS::Socket & socket) : socket(socket) {
	}
	
	ChunkedReader::~ChunkedReader() {
	}
	size_t ChunkedReader::minSize(size_t a, size_t b) {
		return (a < b ? a : b);
	}
	size_t ChunkedReader::readChunkSize() {
		char ch;
		int len;
		chunkedBuffer.clear();
		while ((len = socket.recv(&ch, 1)) > 0) {
			chunkedBuffer.readChunkSize(ch);
			if (chunkedBuffer.hasSizeRecognized()) {
				return chunkedBuffer.getChunkSize();
			}
		}
		return 0;
	}
	size_t ChunkedReader::readChunkData(char * out, size_t maxSize) {

		int len = 0;
		if (chunkedBuffer.getChunkSize() > 0) {
			char readBuffer[1024] = {0,};
			while (!chunkedBuffer.completeData() && (len = socket.recv(readBuffer, chunkedBuffer.getReadSize(sizeof(readBuffer)))) > 0) {
				chunkedBuffer.readChunkData(readBuffer, len);
			}
		}

		// read trailing \r\n
		ConsumeBuffer consume(2);
		char ch;
		while (!consume.complete() && (len = socket.recv(&ch, 1)) > 0) {
			consume.read(len);
		}

		size_t writeSize = minSize(maxSize, chunkedBuffer.getChunkSize());
		memcpy(out, chunkedBuffer.getChunkData(), writeSize);

		return writeSize;
	}
	size_t ChunkedReader::read(char * buffer, size_t max) {
		readChunkSize();
		return readChunkData(buffer, max);
	}
}
