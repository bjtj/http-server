libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = \
	ChunkedReader.hpp \
	HttpClient.hpp \
	HttpClientThreadPool.hpp \
	HttpConnection.hpp \
	HttpHeader.hpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.hpp \
	HttpParameter.hpp \
	HttpProtocol.hpp \
	HttpRequest.hpp \
	HttpResponse.hpp \
	HttpServer.hpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	Logger.hpp \
	MultiConn.hpp \
	OnHttpRequestHandler.hpp \
	Url.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	ChunkedReader.cpp \
	ChunkedReader.hpp \
	HttpClient.cpp \
	HttpClient.hpp \
	HttpClientThreadPool.cpp \
	HttpClientThreadPool.hpp \
	HttpConnection.cpp \
	HttpConnection.hpp \
	HttpHeader.cpp \
	HttpHeader.hpp \
	HttpHeaderParser.cpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.cpp \
	HttpHeaderReader.hpp \
	HttpParameter.cpp \
	HttpParameter.hpp \
	HttpProtocol.cpp \
	HttpProtocol.hpp \
	HttpRequest.cpp \
	HttpRequest.hpp \
	HttpResponse.cpp \
	HttpResponse.hpp \
	HttpServer.cpp \
	HttpServer.hpp \
	HttpStatusCodes.cpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	Logger.cpp \
	Logger.hpp \
	MultiConn.cpp \
	MultiConn.hpp \
	OnHttpRequestHandler.hpp \
	Url.cpp \
	Url.hpp

libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
