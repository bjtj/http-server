#include "DataTransfer.hpp"

namespace http {

	using namespace osl;
    
    DataTransfer::DataTransfer() : _completed(false) {
    }
	DataTransfer::DataTransfer(AutoRef<DataSource> source) : _completed(false), _source(source) {
	}
	DataTransfer::DataTransfer(AutoRef<DataSink> sink) : _completed(false), _sink(sink) {
	}
    DataTransfer::~DataTransfer() {
    }

	unsigned long long DataTransfer::size() {
		throw NotImplementedException("Not implememented", -1, 0);
	}
    void DataTransfer::complete() {
        _completed = true;
    }
    bool DataTransfer::completed() {
        return _completed;
    }
	AutoRef<DataSource> & DataTransfer::source() {
		return _source;
	}
	AutoRef<DataSink> & DataTransfer::sink() {
		return _sink;
	}
}
