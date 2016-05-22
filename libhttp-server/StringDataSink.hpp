#ifndef __STRING_DATA_SINK_HPP__
#define __STRING_DATA_SINK_HPP__

#include "DataSink.hpp"

namespace HTTP {
	
	class StringDataSink : public DataSink {
	private:
		std::string _data;
	public:
		StringDataSink() {}
		virtual ~StringDataSink() {}
		virtual size_t write(const char * data, size_t size) {
			_data.append(data, size);
			return size;
		}
		std::string & data() {
			return _data;
		}
		static std::string & data(DataSink * sink) {
			return ((StringDataSink*)sink)->data();
		}
	};
}

#endif
