#ifndef __CHUNKED_READER_HPP__
#define __CHUNKED_READER_HPP__

#include <liboslayer/os.hpp>
#include <string>

namespace HTTP {
	
	/**
	 * @brief Chunked Read Buffer
	 */
	class ChunkedReaderBuffer {
	private:
		std::string chunkSizeBuffer;
		char * chunkDataBuffer;
		size_t chunkSize;
		bool chunkSizeRecognized;
		size_t chunkDataReadPosition;

	public:
		ChunkedReaderBuffer();
		virtual ~ChunkedReaderBuffer();

		void clear();
		void readChunkSize(char ch);
		void readChunkData(const char * data, size_t len);
		size_t remainingDataBuffer() const;
		bool hasSizeRecognized() const;
		bool completeData() const;
		int getChunkSize() const;
		const char * getChunkData() const;
		size_t getReadSize(size_t bufferSize) const;
	};

	/**
	 * @brief consume
	 */
	class ConsumeBuffer {
	private:
		size_t pos;
		size_t maxSize;
	public:
		ConsumeBuffer(size_t maxSize);
		virtual ~ConsumeBuffer();

		void clear();
		void read(size_t len);
		size_t remaining();
		bool complete();

		void setMaxSize(size_t maxSize);
	};

	/**
	 * @brief Chunked Reader
	 */
	class ChunkedReader {
	private:
		OS::Socket & socket;
		ChunkedReaderBuffer chunkedBuffer;
		
	public:
		ChunkedReader(OS::Socket & socket);
		virtual ~ChunkedReader();

		size_t minSize(size_t a, size_t b);
		int readChunkSize();
		size_t readChunkData(char * out, size_t max);
		size_t read(char * buffer, size_t max);
		
	};
}

#endif
