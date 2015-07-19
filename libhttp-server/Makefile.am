lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	os.cpp \
	Text.cpp \
	Url.cpp \
	MultiConn.cpp \
	HttpConnection.cpp \
	HttpHeader.cpp \
	HttpProtocol.cpp \
	HttpStatusCodes.cpp

libhttp_server_la_CPPFLAGS = -I$(top_srcdir)/include
libhttp_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)

