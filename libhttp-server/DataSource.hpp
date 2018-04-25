#ifndef __DATA_SOURCE_HPP__
#define __DATA_SOURCE_HPP__

namespace http {
	
	class DataSource {
	private:
	public:
		DataSource(){}
		virtual ~DataSource() {}
		virtual size_t read(char * buffer, size_t size) = 0;
		virtual bool eof() = 0;
	};
}

#endif
