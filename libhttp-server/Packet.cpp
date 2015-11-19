#include "Packet.hpp"

namespace HTTP {
    
    using namespace OS;

    Packet::Packet(size_t size) : buffer(NULL), size(size), limit(size) {
        if (size > 0) {
            buffer = new char[size];
            memset(buffer, 0, size);
        }
    }
    Packet::Packet(char * buffer, size_t size) : buffer(buffer), size(size), limit(size){
    }
    Packet::Packet(const Packet & other) {
        size = other.size;
        pos = 0;
        limit = other.limit;
        if (size > 0) {
            buffer = new char[size];
            memcpy(buffer, other.buffer, size);
        } else {
            buffer = NULL;
        }
    }
    Packet::~Packet() {
        if (buffer) {
            delete [] buffer;
        }
    }
    Packet Packet::wrap(char * buffer, size_t len) {
        return Packet(buffer, len);
    }
    
    void Packet::clear() {
        memset(buffer, 0, size);
        pos = 0;
    }
    size_t Packet::remaining() {
        return limit - pos;
    }
    void Packet::write(const char * data, size_t len) {
        size_t remain = remaining();
        if (len > remain) {
            throw Exception("overflow", -1, 0);
        }
        
        memcpy(buffer + pos, data, len);
        pos += len;
    }
    char * Packet::getData() {
        return buffer;
    }
    size_t Packet::getLength() {
        return pos;
    }
    void Packet::setPosition(size_t position) {
        if (position > limit) {
            throw Exception("overlimit", -1, 0);
        }
        this->pos = position;
    }
    size_t Packet::getLimit() {
        return limit;
    }
    void Packet::resetLimit() {
        limit = size;
    }
    void Packet::setLimit(size_t limit) {
        if (limit > size) {
            throw Exception("oversize", -1, 0);
        }
        this->limit = limit;
    }
    
}