#ifndef __URL_ENCODER_DECODER_HPP__
#define __URL_ENCODER_DECODER_HPP__

#include <string>

namespace HTTP {

	class UrlEncoder {
	private:
	public:
		UrlEncoder();
		virtual ~UrlEncoder();

		static std::string encode(const std::string & text);
	private:
		static std::string toHexString(int ch);
	};

	class UrlDecoder {
	private:
	public:
		UrlDecoder();
		virtual ~UrlDecoder();

		static std::string decode(const std::string & text);
		static std::string decode_plus(const std::string & text);
	};
}

#endif
