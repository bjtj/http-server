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
		std::string buffer;
	public:
		HttpHeaderReader();
		virtual ~HttpHeaderReader();

		void clear();
		bool complete();
		int cutEndPos();
		void append(const char * data, int size);
		int read(const char * data, int size);
		bool parse();
		HttpHeader & getHeader();
	};

}

#endif
