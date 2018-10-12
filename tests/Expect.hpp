#ifndef __EXPECT_HPP__
#define __EXPECT_HPP__

#include <liboslayer/os.hpp>
#include <liboslayer/Semaphore.hpp>

class Expect
{
private:
	bool _flag;
	int _val;
	int _expect;
	osl::Semaphore _sem;
public:
	Expect(int init, int expect) : _flag(false), _val(init), _expect(expect), _sem(1) {
	}
    Expect(int init) : _flag(false), _val(init), _expect(0), _sem(1) {
	}
    virtual ~Expect() {
	}
	void checkFlag() {
		_sem.wait();
		if (_val == _expect) {
			_flag = true;
		}
		_sem.post();
	}
	Expect & operator++() {
		_val++;
		checkFlag();
		return *this;
	}
	Expect operator++(int) {
		Expect ret = *this;
		++*this;
		return ret;
	}
	Expect & operator--() {
		_val--;
		checkFlag();
		return *this;
	}
	Expect operator--(int) {
		Expect ret = *this;
		--*this;
		return ret;
	}
	bool & flag() {
		return _flag;
	}
	void wait() {
		while (_flag == false) {
			osl::idle(10);
		}
	}
	int diff() {
		return _expect - _val;
	}
		
};

#endif
