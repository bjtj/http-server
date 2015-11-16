#include "Packet.hpp"

namespace HTTP {

	Packet::Packet() : buffer(NULL), _size(0), _length(0) {
	}

	Packet::Packet(int size) : _size(size), _length(0) {
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
	}
    
	Packet::Packet(char * buffer, int size) : _size(size), _length(_size) {
		if (buffer) {
			this->buffer = (char*)malloc(_size);
			memcpy(this->buffer, buffer, _size);
		} else {
			this->buffer = NULL;
		}
	}

	Packet::Packet(const Packet & other) {
		this->_size = other._size;
		this->_length = other._length;
		if (this->_size > 0) {
			this->buffer = (char*)malloc(this->_size);
			memcpy(this->buffer, other.buffer, this->_size);
		} else {
			this->buffer = NULL;
		}
	}
    
	Packet::~Packet() {
		if (buffer) {
			free(buffer);
		}
	}
    void Packet::clear() {
		if (buffer) {
			memset(buffer, 0, _size);
		}
		_length = 0;
	}
	char * Packet::getBuffer() {
		return buffer;
	}
    
	int Packet::put(char * data, int len) {
		int cap = _size - _length;
		if (!buffer || cap < len) {
			return -1;
		}

		memcpy(buffer + _length, data, len);
		_length += len;

		return _length;
	}
    
	int Packet::size() {
		return _size;
	}
    
	void Packet::resize(int size) {
		if (buffer) {
			free(buffer);
		}
		_size = size;
		buffer = (char*)malloc(_size);
		memset(buffer, 0, _size);
		_length = 0;
	}
    
	int Packet::length() {
		return _length;
	}

	Packet & Packet::operator= (const Packet & other) {
		this->_size = other._size;
		this->_length = other._length;
		if (this->_size > 0) {
			this->buffer = (char*)malloc(this->_size);
			memcpy(this->buffer, other.buffer, this->_size);
		} else {
			this->buffer = NULL;
		}
		return *this;
	}

}