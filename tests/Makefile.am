TESTS=$(check_PROGRAMS)

check_PROGRAMS=test-http-header test-url

test_http_header_SOURCES = test-http-header.cpp
test_http_header_CPPFLAGS = -I$(top_srcdir)
test_http_header_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_http_header_LDFLAGS=$(PTHREAD_CFLAGS)
test_http_header_LDADD = ../libhttp-server/libhttp-server.la

test_url_SOURCES = test-url.cpp
test_url_CPPFLAGS = -I$(top_srcdir)
test_url_header_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_url_LDFLAGS=$(PTHREAD_CFLAGS)
test_url_LDADD = ../libhttp-server/libhttp-server.la
