TESTS=$(check_PROGRAMS)

check_PROGRAMS=\
	test-http-header \
	test-url \
	test-connection-manager \
	test-lisp-page \
	test-buffer-indicator \
	test-data-source \
	test-data-sink \
	test-http-server \
	test-exception-handling \
	test-http-client 

test_http_header_SOURCES = test-http-header.cpp
test_url_SOURCES = test-url.cpp
test_connection_manager_SOURCES = test-connection-manager.cpp
test_lisp_page_SOURCES = test-lisp-page.cpp
test_buffer_indicator_SOURCES = test-buffer-indicator.cpp
test_data_source_SOURCES = test-data-source.cpp
test_data_sink_SOURCES = test-data-sink.cpp
test_http_server_SOURCES = test-http-server.cpp
test_exception_handling_SOURCES = test-exception-handling.cpp
test_http_client_SOURCES = test-http-client.cpp

AM_CPPFLAGS = -I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD = ../libhttp-server/libhttp-server.la
