#ifndef __HTTP_HEADER_READER_HPP__
#define __HTTP_HEADER_READER_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

#include "HttpHeader.hpp"
#include "HttpHeaderParser.hpp"

namespace HTTP {

	/**
	 * @brief http header reader
	 */
	class HttpHeaderReader {
	private:
		HttpHeaderParser parser;
		std::string _buffer;
	public:
		HttpHeaderReader();
		virtual ~HttpHeaderReader();

		void clear();
		bool complete();
		void append(const char * data, size_t size);
		size_t read(const char * data, size_t size);
		bool parse();
		HttpHeader & getHeader();
		std::string & buffer();
	};

}

#endif
