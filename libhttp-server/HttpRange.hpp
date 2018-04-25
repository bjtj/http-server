#ifndef __RANGE_HPP__
#define __RANGE_HPP__

#include <string>
#include <liboslayer/Text.hpp>

namespace http {

	
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
		void adjustTo(size_t size) {
			if (_to == 0 || _to >= size) {
				_to = size - 1;
			}
		}
		size_t size() const  {
			return _to - _from + 1;
		}
        
        std::string toString() const {
			std::string str;
			str.append(osl::Text::toString(_from));
			str.append("-");
			str.append(osl::Text::toString(_to));
			return str;
        }
	};

}

#endif
