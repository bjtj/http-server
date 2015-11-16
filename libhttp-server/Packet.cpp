#include "Packet.hpp"

namespace HTTP {
    
    using namespace OS;

//	Packet::Packet() : buffer(NULL), _size(0), _length(0) {
//	}
//
//	Packet::Packet(int size) : _size(size), _length(0) {
//		buffer = (char*)malloc(_size);
//		memset(buffer, 0, _size);
//	}
//    
//	Packet::Packet(char * buffer, int size) : _size(size), _length(_size) {
//		if (buffer) {
//			this->buffer = (char*)malloc(_size);
//			memcpy(this->buffer, buffer, _size);
//		} else {
//			this->buffer = NULL;
//		}
//	}
//
//	Packet::Packet(const Packet & other) {
//		this->_size = other._size;
//		this->_length = other._length;
//		if (this->_size > 0) {
//			this->buffer = (char*)malloc(this->_size);
//			memcpy(this->buffer, other.buffer, this->_size);
//		} else {
//			this->buffer = NULL;
//		}
//	}
//    
//	Packet::~Packet() {
//		if (buffer) {
//			free(buffer);
//		}
//	}
//    void Packet::clear() {
//		if (buffer) {
//			memset(buffer, 0, _size);
//		}
//		_length = 0;
//	}
//	char * Packet::getBuffer() {
//		return buffer;
//	}
//    
//	int Packet::put(char * data, int len) {
//		int cap = _size - _length;
//		if (!buffer || cap < len) {
//			return -1;
//		}
//
//		memcpy(buffer + _length, data, len);
//		_length += len;
//
//		return _length;
//	}
//    
//	int Packet::size() {
//		return _size;
//	}
//    
//	void Packet::resize(int size) {
//		if (buffer) {
//			free(buffer);
//		}
//		_size = size;
//		buffer = (char*)malloc(_size);
//		memset(buffer, 0, _size);
//		_length = 0;
//	}
//    
//	int Packet::length() {
//		return _length;
//	}
//
//	Packet & Packet::operator= (const Packet & other) {
//		this->_size = other._size;
//		this->_length = other._length;
//		if (this->_size > 0) {
//			this->buffer = (char*)malloc(this->_size);
//			memcpy(this->buffer, other.buffer, this->_size);
//		} else {
//			this->buffer = NULL;
//		}
//		return *this;
//	}

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