#ifndef __RANGE_HPP__
#define __RANGE_HPP__

#include <string>
#include <liboslayer/Text.hpp>

namespace HTTP {

	
	/**
	 * 
	 */
	class HttpRange {
	private:
		size_t _from;
		size_t _to;
	public:
		HttpRange() : _from(0), _to(0) {
		}
		HttpRange(size_t from, size_t to) : _from(from), _to(to) {
		}
		virtual ~HttpRange() {
		}
		size_t & from() {
			return _from;
		}
		size_t & to() {
			return _to;
		}
		size_t from() const {
			return _from;
		}
		size_t to() const {
			return _to;
		}
		size_t size() const  {
			return _to - _from;
		}
        
        std::string toString() const {
            return UTIL::Text::format("%d ~ %d", _from, _to);
        }
	};

}

#endif
