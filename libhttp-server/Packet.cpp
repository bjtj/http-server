#include "Packet.hpp"
#include <liboslayer/Text.hpp>

namespace http {
    
	using namespace osl;

    Packet::Packet(size_t capacity) : buffer(NULL), _capacity(capacity), _pos(0), _limit(capacity) {
        if (_capacity > 0) {
            buffer = new char[_capacity];
			clear();
        }
    }
    Packet::Packet(char * buffer, size_t capacity) : buffer(buffer), _capacity(capacity), _pos(0), _limit(capacity){
		clear();
    }
    Packet::Packet(const Packet & other) {
        _capacity = other._capacity;
        _pos = 0;
        _limit = other._limit;
        if (_capacity > 0) {
            buffer = new char[_capacity];
            memcpy(buffer, other.buffer, _capacity);
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
        memset(buffer, 0, _capacity);
        _pos = 0;
    }
    size_t Packet::remaining() {
        return (_limit - _pos);
    }
    void Packet::write(const char * data, size_t len) {
        size_t remain = remaining();
        if (len > remain) {
            throw Exception("overflow", -1, 0);
        }
        
        memcpy(buffer + _pos, data, len);
        _pos += len;
    }
    char * Packet::getData() {
        return buffer;
    }
    size_t Packet::getLength() {
        return _pos;
    }
    void Packet::setPosition(size_t position) {
        if (position > _limit) {
            throw Exception("postion (" + Text::toString(position) + ") is out of limit (" +
							Text::toString(_limit) + ")");
        }
        this->_pos = position;
    }
    size_t Packet::getLimit() {
        return _limit;
    }
    void Packet::restoreLimit() {
        _limit = _capacity;
    }
    void Packet::setLimit(size_t limit) {
        if (limit > _capacity) {
			throw Exception("limit (" + Text::toString(_pos) + ") is out of capacity (" +
							Text::toString(limit) + ")");
        }
        this->_limit = limit;
    }

	size_t Packet::capacity() {
		return _capacity;
	}
}
