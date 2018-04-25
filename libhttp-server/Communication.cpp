#include "Communication.hpp"

namespace http {
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
