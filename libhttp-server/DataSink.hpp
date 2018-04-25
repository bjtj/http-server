#ifndef __DATA_SINK_HPP__
#define __DATA_SINK_HPP__

namespace http {
	
	class DataSink {
	private:
	public:
		DataSink() {}
		virtual ~DataSink() {}
		virtual size_t write(const char * data, size_t size) = 0;
	};
}

#endif
