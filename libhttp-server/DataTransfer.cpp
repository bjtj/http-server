#include "DataTransfer.hpp"

namespace HTTP {
    
    DataTransfer::DataTransfer() : completed(false) {
    }
    
    DataTransfer::~DataTransfer() {
    }

	unsigned long long DataTransfer::getSize() {
		throw OS::NotImplementedException("Not implememented", -1, 0);
	}
    
    void DataTransfer::setCompleted() {
        completed = true;
    }
    bool DataTransfer::isCompleted() {
        return completed;
    }
    
}