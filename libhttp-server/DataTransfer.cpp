#include "DataTransfer.hpp"

namespace HTTP {
    
    DataTransfer::DataTransfer() : completed(false) {
    }
    
    DataTransfer::~DataTransfer() {
    }
    
    void DataTransfer::setCompleted() {
        completed = true;
    }
    bool DataTransfer::isCompleted() {
        return completed;
    }
    
}