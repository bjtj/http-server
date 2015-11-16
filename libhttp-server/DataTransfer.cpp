#include "DataTransfer.hpp"

namespace HTTP {
    
    DataTransfer::DataTransfer() {
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