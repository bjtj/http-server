#ifndef __HTTP_ENCODER_DECODER_HPP__
#define __HTTP_ENCODER_DECODER_HPP__

#include <string>

namespace HTTP {

	class HttpEncoder {
	private:
	public:
		HttpEncoder();
		virtual ~HttpEncoder();

		static std::string encode(const std::string & text);
	private:
		static std::string toHexString(int ch);
	};

	class HttpDecoder {
	private:
	public:
		HttpDecoder();
		virtual ~HttpDecoder();

		static std::string decode(const std::string & text);
	};
}

#endif