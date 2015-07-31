libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = \
	Text.hpp \
	Url.hpp \
	MultiConn.hpp \
	HttpParameter.hpp \
	HttpHeader.hpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.hpp \
	HttpRequest.hpp \
	HttpResponse.hpp \
	OnHttpRequestHandler.hpp \
	HttpProtocol.hpp \
	HttpStatusCodes.hpp \
	HttpConnection.hpp \
	Http.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	Text.hpp \
	Url.hpp \
	MultiConn.hpp \
	HttpParameter.hpp \
	HttpHeader.hpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.hpp \
	HttpRequest.hpp \
	HttpRequest.cpp \
	HttpResponse.hpp \
	HttpResponse.cpp \
	HttpProtocol.hpp \
	HttpStatusCodes.hpp \
	HttpConnection.hpp \
	Text.cpp \
	Url.cpp \
	MultiConn.cpp \
	HttpConnection.cpp \
	HttpParameter.cpp \
	HttpHeader.cpp \
	HttpHeaderParser.cpp \
	HttpHeaderReader.cpp \
	HttpProtocol.cpp \
	HttpStatusCodes.cpp \
	Http.hpp

libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
