#include "HttpEncoderDecoder.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static string num = "0123456789";
	static string hex = "0123456789abcdef";
	static string alpha = "abcdefghijklmnopqrstuvwxyz";
	static string alphaBig = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static string special = "@./\\-:";
	static string known = num + hex + alpha + alphaBig + special;

	HttpEncoder::HttpEncoder() {
	}

	HttpEncoder::~HttpEncoder() {
	}

	string HttpEncoder::encode(const string & text) {
		
		string ret;
		size_t f = 0;
		while (1) {
			size_t e = text.find_first_not_of(known, f);

			if (e != string::npos) {
				ret.append(text.substr(f, e - f));
				ret.append("%" + toHexString(text[e]));
				f = e + 1;
			} else {
				ret.append(text.substr(f));
				break;
			}
		}

		return ret;
	}

	string HttpEncoder::toHexString(int ch) {
		string hex = Text::toUpperHexString(ch);
		if (hex.length() < 2) {
			return "0" + hex;
		} else if (hex.length() > 2) {
			return hex.substr(hex.length() - 2);
		}

		return hex;
	}


	HttpDecoder::HttpDecoder() {
	}

	HttpDecoder::~HttpDecoder() {
	}

	string HttpDecoder::decode(const string & text) {

		string ret;
		size_t f = 0;
		while (1) {
			size_t e = text.find("%", f);

			if (e != string::npos) {
				ret.append(text.substr(f, e - f));
				int ch = Text::toInt(text.substr(e + 1, 2), 16);
				ret.append(1, ch);
				f = e + 3;
			} else {
				ret.append(text.substr(f));
				break;
			}
		}

		return ret;
	}

	string HttpDecoder::decode_plus(const string & text) {
		return Text::replaceAll(text, "+", " ");
	}
}
