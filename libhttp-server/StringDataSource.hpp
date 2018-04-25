#ifndef __STRING_DATA_SOURCE_HPP__
#define __STRING_DATA_SOURCE_HPP__

#include <liboslayer/os.hpp>
#include "DataSource.hpp"
#include "BufferIndicator.hpp"
#include <string>
#include <cstring>

namespace http {
	
	class StringDataSource : public DataSource {
	private:
		std::string _data;
		BufferIndicator indicator;
	public:
		StringDataSource(const std::string & data) : _data(data), indicator(data.size()) {}
		virtual ~StringDataSource() {}
		virtual size_t read(char * buffer, size_t size) {
			size_t readSize = indicator.adjustReadSize(size);
			if (readSize > 0) {
				std::string chunk = _data.substr(indicator.position(), readSize);
				memcpy(buffer, chunk.c_str(), readSize);
				indicator.offset(readSize);
			}
			return readSize;
		}
		virtual bool eof() {
			return indicator.completed();
		}
	};

}

#endif
