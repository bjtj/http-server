lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	Text.cpp \
	Url.cpp \
	MultiConn.cpp \
	HttpConnection.cpp \
	HttpHeader.cpp \
	HttpProtocol.cpp \
	HttpStatusCodes.cpp

libhttp_server_la_CPPFLAGS = -I$(top_srcdir)/include
libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
