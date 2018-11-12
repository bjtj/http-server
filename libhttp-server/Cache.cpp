#include "Cache.hpp"
#include <liboslayer/os.hpp>

using namespace std;
using namespace osl;


namespace http {

    Cache::Cache(const string & uid, const string & path, unsigned long expirationDate)
	: _uid(uid),
	  _path(path),
	  _data(NULL),
	  _size(0),
	  _createDate(tick_milli()),
	  _expirationDate(expirationDate) {
    }

    Cache::~Cache() {
    }

    string & Cache::uid() {
	return _uid;
    }

    string & Cache::path() {
	return _path;
    }

    unsigned long & Cache::createDate() {
	return _createDate;
    }

    unsigned long & Cache::expirationDate() {
	return _expirationDate;
    }

    void * Cache::getData() {
	return _data;
    }

    void Cache::setData(void * data, size_t size) {
	_data = data;
	_size = size;
    }

    string Cache::uid() const {
	return _uid;
    }

    string Cache::path() const {
	return _path;
    }

    unsigned long Cache::createDate() const {
	return _createDate;
    }

    unsigned long Cache::expirationDate() const {
	return _expirationDate;
    }

    bool Cache::expired() const {
	if (_expirationDate == 0) {
	    return false;
	}
	return (tick_milli() >= _expirationDate);
    }
}
