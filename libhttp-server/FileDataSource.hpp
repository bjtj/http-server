#ifndef __FILE_DATA_SOURCE_HPP__
#define __FILE_DATA_SOURCE_HPP__

#include "DataSource.hpp"
#include <liboslayer/FileStream.hpp>

namespace http {
	
	class FileDataSource : public DataSource {
	private:
		osl::FileStream _fstream;
	public:
		FileDataSource(osl::FileStream fstream) : _fstream(fstream) {
		}
		virtual ~FileDataSource() {
			_fstream.close();
		}
		virtual size_t read(char * buffer, size_t size) {
			return _fstream.read(buffer, size);
		}
		virtual bool eof() {
			return _fstream.eof();
		}
	};

	
}

#endif
