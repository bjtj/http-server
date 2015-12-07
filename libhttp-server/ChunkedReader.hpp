#ifndef __CHUNKED_READER_HPP__
#define __CHUNKED_READER_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <string>

namespace HTTP {
    
    /**
     * @brief Chunked Buffer
     */
    class ChunkedBuffer {
    private:
        char * chunkDataBuffer;
        size_t chunkSize;
        size_t pos;

    public:
        ChunkedBuffer();
		ChunkedBuffer(const ChunkedBuffer & other);
        virtual ~ChunkedBuffer();
        
        void clear();
        size_t read(char * data, size_t len);
        void write(const char * data, size_t len);
        void resetPosition();
        size_t remainingDataBuffer() const;
		bool remain() const;
        bool completeData() const;
        void setChunkSize(size_t size);
        size_t getChunkSize() const;
        const char * getChunkData() const;
        size_t getReadableSize(size_t bufferSize) const;
        size_t getPosition() const;
        void setPosition(size_t pos);
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
	 * @brief ReadCounter
	 */
	class ReadCounter {
	private:
		size_t pos;
		size_t contentSize;
	public:
		ReadCounter();
		ReadCounter(size_t contentSize);
		virtual ~ReadCounter();

		void resetPosition();
		void read(size_t len);
		size_t remaining() const;
		bool complete() const;

		void setContentSize(size_t contentSize);
		size_t getContentSize();
        size_t getReadSize(size_t bufferSize) const;
		size_t getReadPosition();
	};


	/**
	 * @brief ReadCounter
	 */
	class LargeReadCounter {
	private:
		unsigned long long pos;
		unsigned long long contentSize;
	public:
		LargeReadCounter();
		LargeReadCounter(unsigned long long contentSize);
		virtual ~LargeReadCounter();

		void resetPosition();
		void read(unsigned long long len);
		unsigned long long remaining() const;
		bool complete() const;

		void setContentSize(unsigned long long contentSize);
		unsigned long long getContentSize();
        unsigned long long getReadSize(unsigned long long bufferSize) const;
		unsigned long long getReadPosition();
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
