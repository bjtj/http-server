#ifndef __CACHE_H__
#define __CACHE_H__

#include <string>

namespace http {

    class Cache
    {
    private:
	std::string _uid;
	std::string _path;
	void * _data;
	size_t _size;
	unsigned long _createDate;
	unsigned long _expirationDate;
    public:
	Cache(const std::string & uid, const std::string & path, unsigned long expirationDate);
	virtual ~Cache();

	std::string & uid();
	std::string & path();
	unsigned long & createDate();
	unsigned long & expirationDate();

	void * getData();
	void setData(void * data, size_t size);

	std::string uid() const;
	std::string path() const;
	unsigned long createDate() const;
	unsigned long expirationDate() const;

	bool expired() const;
    };

}

#endif
