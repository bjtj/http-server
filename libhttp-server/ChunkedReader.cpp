#include "ChunkedReader.hpp"

#include <liboslayer/Text.hpp>
#include <string>

#include "Logger.hpp"

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static Logger & logger = Logger::getLogger();
	
	ChunkedReader::ChunkedReader(OS::Socket & socket) : socket(socket) {
	}
	
	ChunkedReader::~ChunkedReader() {
	}
	
	int ChunkedReader::readChunkSize() {
		char ch;
		string buf;
		int len;
		while ((len = socket.recv(&ch, 1)) > 0) {
			size_t f;
			buf.append(&ch, 1);
			f = buf.find("\r\n");
			if (f != string::npos) {
				return Text::toInt(buf, 16);
			}
		}
		return 0;
	}
	int ChunkedReader::readChunkData(int chunkSize, char * out, int max) {
		
		int len = 0;
		char readBuf[1024] = {0,};
		int readSize;
		int readPos = 0;
		int writePos = 0;
		
		while (readSize = calcBufferedSize(chunkSize + 2, readPos, sizeof(readBuf)),
			   (readSize > 0) && ((len = socket.recv(readBuf, readSize)) > 0)) {

			readPos += len;

			int writeSize = calcBufferedSize(max, writePos, len);
			if (writeSize > 0) {
				memcpy(out + writePos, readBuf, writeSize);
				writePos += len;
			} else {
				writePos = max;
			}
		}

		if (readPos >= 2 && writePos > readPos - 2) {
			writePos = readPos - 2;
		}

		return writePos;
	}
	int ChunkedReader::read(char * buffer, int max) {
		int size = readChunkSize();
		return readChunkData(size, buffer, max);
	}

	int ChunkedReader::calcBufferedSize(int max, int pos, int bufferSize) {
		if (pos + bufferSize > max) {
			int ret = bufferSize - (pos + bufferSize - max);
			return ret;
		}

		return bufferSize;
	}
}
