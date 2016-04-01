#ifndef __TEST_UTIL_HPP__
#define __TEST_UTIL_HPP__

#include <cstdlib>
#include <cstdio>
#include <iostream>

#define ASSERT(A,CMP,B)													\
	std::cout << #A << "(" << #CMP << " " << B << ")";					\
	if (!(A CMP B)) {													\
		std::cout << " - FAIL" << std::endl;							\
		cerr << " <!> " << #A <<  " should be [" << #CMP << " " <<  B << "] but [" << A << "]" << std::endl; \
		exit(1);														\
	} else {															\
		std::cout << " - PASS" << std::endl;							\
	}

#endif
