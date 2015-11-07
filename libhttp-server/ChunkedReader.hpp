#ifndef __CHUNKED_READER_HPP__
#define __CHUNKED_READER_HPP__

#include <liboslayer/os.hpp>
#include <string>

namespace HTTP {
    
    /**
     * @brief Chunked Buffer
     */
    class ChunkedBuffer {
    private:
        char * chunkDataBuffer;
        size_t chunkSize;
        size_t chunkDataReadPosition;
        
    public:
        ChunkedBuffer();
        virtual ~ChunkedBuffer();
        
        void clear();
        void readChunkData(const char * data, size_t len);
        size_t remainingDataBuffer() const;
        bool completeData() const;
        void setChunkSize(size_t size);
        size_t getChunkSize() const;
        const char * getChunkData() const;
        size_t getReadSize(size_t bufferSize) const;
        size_t getCurrentReadPosition() const;
    };
	
	/**
	 * @brief Chunked Read Buffer
	 */
    class ChunkedReaderBuffer : public ChunkedBuffer {
	private:
		std::string chunkSizeBuffer;
		bool chunkSizeRecognized;

	public:
		ChunkedReaderBuffer();
		virtual ~ChunkedReaderBuffer();

		void clear();
		void readChunkSize(char ch);
		bool hasSizeRecognized() const;
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
		size_t remaining() const;
		bool complete() const;

		void setMaxSize(size_t maxSize);
        size_t getReadSize(size_t bufferSize) const;
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
		size_t readChunkSize();
		size_t readChunkData(char * out, size_t max);
		size_t read(char * buffer, size_t max);
		
	};
}

#endif
