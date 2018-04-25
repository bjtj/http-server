#include "MimeTypes.hpp"
#include <liboslayer/FileStream.hpp>

namespace http {

	using namespace std;
	using namespace osl;

	map<string, string> MimeTypes::types;
	
	MimeTypes::MimeTypes() {
	}
	MimeTypes::~MimeTypes() {
	}

	void MimeTypes::clear() {
		types.clear();
	}
	void MimeTypes::load(const string & path) {
		FileStream in(path, "rb");
		while (!in.eof()) {
			string line = in.readline();
			size_t tab = line.find("\t");
			string ext = line.substr(0, tab);
			string type = line.substr(tab+1);
			types[ext] = type;
		}
		in.close();
	}

	map<string, string> MimeTypes::getMimeTypes() {
		return types;
	}
	
	string MimeTypes::getMimeType(const std::string & ext) {
		return types[ext];
	}
}
