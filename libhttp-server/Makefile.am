lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	Text.cpp \
	Url.cpp \
	MultiConn.cpp \
	HttpConnection.cpp \
	HttpHeader.cpp \
	HttpProtocol.cpp \
	HttpStatusCodes.cpp

libhttp_server_la_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/deps/oslayer/liboslayer
libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS)
libhttp_server_la_LIBADD = ../deps/oslayer/liboslayer/liboslayer.la

