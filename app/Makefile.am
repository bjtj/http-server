bin_PROGRAMS=http-server
http_server_SOURCES= \
	HttpRequestHandlers.hpp HttpRequestHandlers.cpp \
	main.cpp
http_server_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/deps/oslayer/liboslayer
http_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)
http_server_LDFLAGS=$(PTHREAD_CFLAGS)
http_server_LDADD = ../libhttp-server/libhttp-server.la
