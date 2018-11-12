#include "CacheManager.hpp"
#include <liboslayer/Text.hpp>

namespace http {

    using namespace std;
    using namespace osl;

    static unsigned int _idx = 0;

    static string genUid() {
	return "cache-uid:" + Text::toString(tick_milli()) + ":" + Text::toString(++_idx);
    }

    CacheManager::CacheManager() {
    }

    CacheManager::~CacheManager() {
    }

    string CacheManager::addCache(const string & path, void * data, size_t size, unsigned long expirationDate) {
	string uid = genUid();
	Cache cache(uid, path, expirationDate);
	cache.setData(data, size);
	_caches.push_back(cache);
	return uid;
    }

    Optional<Cache> CacheManager::getCache(const string & uid) {
	Optional<Cache> ret;
	_lock.lock();
	for (vector<Cache>::iterator iter = _caches.begin()
	     ; iter != _caches.end(); ++iter)
	{
	    if (iter->uid() == uid) {
		ret = Optional<Cache>(*iter);
		break;
	    }
	}
	_lock.unlock();
	return ret;
    }

    vector<Cache> CacheManager::getCachesByPath(const string & path) {
	vector<Cache> vec;
	_lock.lock();
	for (vector<Cache>::iterator iter = _caches.begin()
	     ; iter != _caches.end(); ++iter)
	{
	    if (iter->path() == path) {
		vec.push_back(*iter);
	    }
	}
	_lock.unlock();
	return vec;
    }

    void CacheManager::resolveExpirations() {
	_lock.lock();
	vector<Cache>::iterator iter = _caches.begin();
	while (iter != _caches.end()) {
	    if (iter->expired()) {
		iter = _caches.erase(iter);
	    } else {
		iter++;
	    }
	}
	_lock.unlock();
    }

}
