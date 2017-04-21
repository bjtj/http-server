#include "UrlEncoderDecoder.hpp"
#include <liboslayer/Text.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static string _num = "0123456789";
	static string _hex = "0123456789abcdef";
	static string _alpha = "abcdefghijklmnopqrstuvwxyz";
	static string _alphaBig = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static string _special = "@./\\-:";
	static string _known = _num + _hex + _alpha + _alphaBig + _special;

	UrlEncoder::UrlEncoder() {
	}

	UrlEncoder::~UrlEncoder() {
	}

	string UrlEncoder::encode(const string & text) {
		string ret;
		size_t f = 0;
		while (1) {
			size_t e = text.find_first_not_of(_known, f);
			if (e != string::npos) {
				ret.append(text.substr(f, e - f));
				ret.append(toHexString(text[e]));
				f = e + 1;
			} else {
				ret.append(text.substr(f));
				break;
			}
		}
		return ret;
	}

	string UrlEncoder::encode_plus(const string & text) {
		string ret;
		size_t f = 0;
		while (1) {
			size_t e = text.find_first_not_of(_known, f);
			if (e != string::npos) {
				ret.append(text.substr(f, e - f));
				if (text[e] == ' ') {
					ret.append("+");
				} else {
					ret.append(toHexString(text[e]));
				}
				f = e + 1;
			} else {
				ret.append(text.substr(f));
				break;
			}
		}
		return ret;
	}

	string UrlEncoder::toHexString(int ch) {
		return Text::nformat(5, "%%%02X", ch & 0xff);
	}

	UrlDecoder::UrlDecoder() {
	}

	UrlDecoder::~UrlDecoder() {
	}

	string UrlDecoder::decode(const string & text) {
		string ret;
		size_t f = 0;
		while (1) {
			size_t e = text.find("%", f);
			if (e != string::npos) {
				if (text.size() - e < 2) {
					ret.append(text.substr(e));
					break;
				}
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

	string UrlDecoder::decode_plus(const string & text) {
		return decode(Text::replaceAll(text, "+", " "));
	}
}
