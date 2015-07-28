libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = \
	Text.hpp \
	Url.hpp \
	MultiConn.hpp \
	HttpHeader.hpp \
	HttpProtocol.hpp \
	HttpStatusCodes.hpp \
	HttpConnection.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	Text.hpp \
	Url.hpp \
	MultiConn.hpp \
	HttpHeader.hpp \
	HttpProtocol.hpp \
	HttpStatusCodes.hpp \
	HttpConnection.hpp \
	Text.cpp \
	Url.cpp \
	MultiConn.cpp \
	HttpConnection.cpp \
	HttpHeader.cpp \
	HttpProtocol.cpp \
	HttpStatusCodes.cpp

libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
