#ifndef __CHUNKED_READER_HPP__
#define __CHUNKED_READER_HPP__

#include <liboslayer/os.hpp>

namespace HTTP {
	
	class ChunkedReader {
	private:
		OS::Socket & socket;
		
	public:
		ChunkedReader(OS::Socket & socket);
		virtual ~ChunkedReader();
		int readChunkSize();
		int readChunkData(int size, char * out, int max);
		int read(char * buffer, int max);
		
	private:
		int calcBufferedSize(int max, int pos, int bufferSize);
	};
}

#endif
