#ifndef __CACHE_MANAGER_HPP__
#define __CACHE_MANAGER_HPP__

#include "Cache.hpp"
#include <vector>
#include <liboslayer/Optional.hpp>
#include <liboslayer/Mutex.hpp>

namespace http {

    class CacheManager
    {
    private:
	osl::Mutex _lock;
	std::vector<Cache> _caches;
    public:
	CacheManager();
	virtual ~CacheManager();
	std::string addCache(const std::string & path, void * data, size_t size, unsigned long expirationDate);
	osl::Optional<Cache> getCache(const std::string & uid);
	std::vector<Cache> getCachesByPath(const std::string & path);
	void resolveExpirations();
    };
}

#endif
