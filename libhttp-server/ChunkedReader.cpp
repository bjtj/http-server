#include "ChunkedReader.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Text.hpp>
#include <string>

#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
    using namespace OS;
	using namespace XOS;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();
    
    ChunkedBuffer::ChunkedBuffer() : chunkDataBuffer(NULL) {
        clear();
    }

    ChunkedBuffer::ChunkedBuffer(const ChunkedBuffer & other) {
		chunkDataBuffer = NULL;
        chunkSize = other.chunkSize;
        pos = other.pos;

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
        pos = 0;
    }
    
    size_t ChunkedBuffer::read(char * data, size_t len) {
        size_t remain = remainingDataBuffer();
        size_t readSize = len;
        if (remain < readSize) {
            readSize = remain;
        }
        
        if (readSize > 0) {
            memcpy(data, chunkDataBuffer + pos, readSize);
            pos += readSize;
        }
        
        return readSize;
    }
    void ChunkedBuffer::write(const char * data, size_t len) {
        size_t remain = remainingDataBuffer();
        if (len > remain) {
            throw Exception("overflow", -1, 0);
        }
        
        memcpy(chunkDataBuffer + pos, data, len);
        pos += len;
    }
    void ChunkedBuffer::resetPosition() {
        pos = 0;
    }
    size_t ChunkedBuffer::remainingDataBuffer() const {
        return (pos <= chunkSize ? chunkSize - pos : 0);
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
		memset(chunkDataBuffer, 0, chunkSize);
		resetPosition();
    }
    
    size_t ChunkedBuffer::getChunkSize() const {
        return chunkSize;
    }
    
    const char * ChunkedBuffer::getChunkData() const {
        return chunkDataBuffer;
    }
    
    size_t ChunkedBuffer::getReadableSize(size_t bufferSize) const {
        size_t remain = remainingDataBuffer();
        return bufferSize > remain ? remain : bufferSize;
    }
    
    size_t ChunkedBuffer::getPosition() const {
        return pos;
    }
    
    void ChunkedBuffer::setPosition(size_t pos) {
        this->pos = pos;
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
	 * @brief ReadCounter
	 */
	ReadCounter::ReadCounter() : pos(0), contentSize(0) {
	}
	ReadCounter::ReadCounter(size_t contentSize) : pos(0), contentSize(contentSize) {
	}
	ReadCounter::~ReadCounter() {
	}

	void ReadCounter::resetPosition() {
		pos = 0;
	}
	void ReadCounter::read(size_t len) {
		size_t writeSize = len < remaining() ? len : remaining();
		pos += writeSize;
	}
	size_t ReadCounter::remaining() const {
		return (pos < contentSize ? contentSize - pos : 0);
	}
	bool ReadCounter::complete() const {
		return remaining() == 0;
	}

	void ReadCounter::setContentSize(size_t contentSize) {
		this->contentSize = contentSize;
	}
	size_t ReadCounter::getContentSize() {
		return contentSize;
	}
    size_t ReadCounter::getReadSize(size_t bufferSize) const {
		size_t remain = remaining();
        return bufferSize > remain ? remain : bufferSize;
	}

	size_t ReadCounter::getReadPosition() {
		return pos;
	}


	/**
	 * @brief ReadCounter
	 */
	LargeReadCounter::LargeReadCounter() : pos(0), contentSize(0) {
	}
	LargeReadCounter::LargeReadCounter(unsigned long long contentSize) : pos(0), contentSize(contentSize) {
	}
	LargeReadCounter::~LargeReadCounter() {
	}

	void LargeReadCounter::resetPosition() {
		pos = 0;
	}
	void LargeReadCounter::read(unsigned long long len) {
		unsigned long long writeSize = len < remaining() ? len : remaining();
		pos += writeSize;
	}
	unsigned long long LargeReadCounter::remaining() const {
		return (pos < contentSize ? contentSize - pos : 0);
	}
	bool LargeReadCounter::complete() const {
		return remaining() == 0;
	}

	void LargeReadCounter::setContentSize(unsigned long long contentSize) {
		this->contentSize = contentSize;
	}
	unsigned long long LargeReadCounter::getContentSize() {
		return contentSize;
	}
    unsigned long long LargeReadCounter::getReadSize(unsigned long long bufferSize) const {
		unsigned long long remain = remaining();
        return bufferSize > remain ? remain : bufferSize;
	}

	unsigned long long LargeReadCounter::getReadPosition() {
		return pos;
	}


	/**
	 * @brief Chunked Reader
	 */
	ChunkedReader::ChunkedReader(Socket & socket) : socket(socket) {
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
			while (!chunkedBuffer.completeData() && (len = socket.recv(readBuffer, chunkedBuffer.getReadableSize(sizeof(readBuffer)))) > 0) {
				chunkedBuffer.write(readBuffer, len);
			}
		}

		// read trailing \r\n
		ReadCounter consume(2);
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
