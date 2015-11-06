#include "ChunkedReader.hpp"

#include <liboslayer/Text.hpp>
#include <string>

#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static const Logger & logger = LoggerFactory::getDefaultLogger();

	/**
	 * @brief Chunked Read Buffer
	 */
	ChunkedReaderBuffer::ChunkedReaderBuffer() : chunkDataBuffer(NULL) {
		clear();
	}
	ChunkedReaderBuffer::~ChunkedReaderBuffer() {
		clear();
	}

	void ChunkedReaderBuffer::clear() {
		chunkSizeBuffer.clear();
		if (chunkDataBuffer) {
			delete [] chunkDataBuffer;
		}
		chunkDataBuffer = NULL;
		chunkSize = 0;
		chunkSizeRecognized = false;
		chunkDataReadPosition = 0;
	}
	void ChunkedReaderBuffer::readChunkSize(char ch) {
		chunkSizeBuffer.append(1, ch);
		if (Text::endsWith(chunkSizeBuffer, "\r\n")) {
			chunkSize = Text::toInt(chunkSizeBuffer, 16);
			chunkDataBuffer = new char[chunkSize];
			chunkSizeRecognized = true;
		}
	}
	void ChunkedReaderBuffer::readChunkData(const char * data, size_t len) {
		size_t remain = remainingDataBuffer();
		size_t writeSize = (len < remain ? len : remain);
		memcpy(chunkDataBuffer + chunkDataReadPosition, data, writeSize);
		chunkDataReadPosition += writeSize;
	}
	size_t ChunkedReaderBuffer::remainingDataBuffer() const {
		return (chunkDataReadPosition <= chunkSize ? chunkSize - chunkDataReadPosition : 0);
	}
	bool ChunkedReaderBuffer::hasSizeRecognized() const {
		return chunkSizeRecognized;
	}
	bool ChunkedReaderBuffer::completeData() const {
		return (remainingDataBuffer() == 0);
	}
	int ChunkedReaderBuffer::getChunkSize() const {
		return chunkSize;
	}
	const char * ChunkedReaderBuffer::getChunkData() const {
		return chunkDataBuffer;
	}

	size_t ChunkedReaderBuffer::getReadSize(size_t bufferSize) const {
		size_t remain = remainingDataBuffer();
		return bufferSize > remain ? remain : bufferSize;
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
	size_t ConsumeBuffer::remaining() {
		return (pos < maxSize ? maxSize - pos : 0);
	}
	bool ConsumeBuffer::complete() {
		return remaining() == 0;
	}

	void ConsumeBuffer::setMaxSize(size_t maxSize) {
		this->maxSize = maxSize;
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
	int ChunkedReader::readChunkSize() {
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
