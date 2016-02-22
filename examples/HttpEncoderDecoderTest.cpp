#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <libhttp-server/HttpEncoderDecoder.hpp>

using namespace std;
using namespace HTTP;

void s_test_encode(const string & text) {
	cout << text << endl;
	string encoded = HttpEncoder::encode(text);
	cout << "encode: " << encoded << endl;
	string decoded = HttpDecoder::decode(encoded);
	cout << "decode: " << decoded << endl;
}

int main(int argc, char * args[]) {

	s_test_encode("c:\\");
	s_test_encode("hello world");
	s_test_encode("!@#$%^&*()_+");
	s_test_encode("http://www.google.com/");
	s_test_encode("ÇÑ±Û");

	getchar();

	return 0;
}
