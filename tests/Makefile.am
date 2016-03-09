TESTS=$(check_PROGRAMS)

check_PROGRAMS=test-http-header test-url test-lisp-page test-http-server

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

test_lisp_page_SOURCES = test-lisp-page.cpp
test_lisp_page_CPPFLAGS = -I$(top_srcdir)
test_lisp_page_header_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_lisp_page_LDFLAGS=$(PTHREAD_CFLAGS)
test_lisp_page_LDADD = ../libhttp-server/libhttp-server.la

test_http_server_SOURCES = test-http-server.cpp
test_http_server_CPPFLAGS = -I$(top_srcdir)
test_http_server_header_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_http_server_LDFLAGS=$(PTHREAD_CFLAGS)
test_http_server_LDADD = ../libhttp-server/libhttp-server.la
