#include "Communication.hpp"

namespace HTTP {
    Communication::Communication() {
    }
    Communication::~Communication() {
    }
	bool Communication::isReadable() {
		return true;
	}
	bool Communication::isWritable() {
		return true;
	}
}
