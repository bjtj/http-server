#include "BufferIndicator.hpp"

namespace HTTP {
	
	BufferIndicator::BufferIndicator() : _position(0), _size(0) {
	}
	BufferIndicator::BufferIndicator(size_t size) : _position(0), _size(size) {
	}
	BufferIndicator::~BufferIndicator() {
	}
	void BufferIndicator::offset(size_t len) {
		size_t writeSize = len < remaining() ? len : remaining();
		_position += writeSize;
	}
	size_t BufferIndicator::remaining() const {
		return (_position < _size ? _size - _position : 0);
	}
	bool BufferIndicator::remain() const {
		return remaining() > 0;
	}
	bool BufferIndicator::completed() const {
		return remaining() == 0;
	}
    size_t BufferIndicator::adjustReadSize(size_t bufferSize) const {
		size_t remain = remaining();
        return bufferSize > remain ? remain : bufferSize;
	}
	size_t BufferIndicator::position() {
		return _position;
	}
	void BufferIndicator::position(size_t position) {
		_position = position;
	}
	size_t BufferIndicator::size() const {
		return _size;
	}
	void BufferIndicator::size(size_t size) {
		_size = size;
	}
}
