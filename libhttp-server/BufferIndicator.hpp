#ifndef __BUFFER_INDICATOR_HPP__
#define __BUFFER_INDICATOR_HPP__

#include <string>

namespace HTTP {
	
	class BufferIndicator {
	private:
		size_t _position;
		size_t _size;
	public:
		BufferIndicator();
		BufferIndicator(size_t size);
		virtual ~BufferIndicator();
		void offset(size_t len);
		size_t remaining() const;
		bool remain() const;
		bool completed() const;
        size_t adjustReadSize(size_t bufferSize) const;
		size_t position();
		void position(size_t position);
		size_t size() const;
		void size(size_t size);
	};
}

#endif
