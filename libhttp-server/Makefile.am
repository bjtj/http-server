libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = AnotherHttpClient.hpp \
	AnotherHttpClientThreadPool.hpp \
	AnotherHttpServer.hpp \
	ChunkedReader.hpp \
	ChunkedTransfer.hpp \
	Communication.hpp \
	Connection.hpp \
	ConnectionManager.hpp \
	ConnectionThreadPool.hpp \
	DataTransfer.hpp \
	FileTransfer.hpp \
	FixedTransfer.hpp \
	HttpClient.hpp \
	HttpClientThreadPool.hpp \
	HttpEncoderDecoder.hpp \
	HttpHeader.hpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.hpp \
	HttpParameter.hpp \
	HttpRequest.hpp \
	HttpResponse.hpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	Packet.hpp \
	Url.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = AnotherHttpClient.cpp \
	AnotherHttpClient.hpp \
	AnotherHttpClientThreadPool.cpp \
	AnotherHttpClientThreadPool.hpp \
	AnotherHttpServer.cpp \
	AnotherHttpServer.hpp \
	ChunkedReader.cpp \
	ChunkedReader.hpp \
	ChunkedTransfer.cpp \
	ChunkedTransfer.hpp \
	Communication.cpp \
	Communication.hpp \
	Connection.cpp \
	Connection.hpp \
	ConnectionManager.cpp \
	ConnectionManager.hpp \
	ConnectionThreadPool.cpp \
	ConnectionThreadPool.hpp \
	DataTransfer.cpp \
	DataTransfer.hpp \
	FileTransfer.cpp \
	FileTransfer.hpp \
	FixedTransfer.cpp \
	FixedTransfer.hpp \
	HttpClient.cpp \
	HttpClient.hpp \
	HttpClientThreadPool.hpp \
	HttpEncoderDecoder.cpp \
	HttpEncoderDecoder.hpp \
	HttpHeader.cpp \
	HttpHeader.hpp \
	HttpHeaderParser.cpp \
	HttpHeaderParser.hpp \
	HttpHeaderReader.cpp \
	HttpHeaderReader.hpp \
	HttpParameter.cpp \
	HttpParameter.hpp \
	HttpRequest.cpp \
	HttpRequest.hpp \
	HttpResponse.cpp \
	HttpResponse.hpp \
	HttpStatusCodes.cpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	Packet.cpp \
	Packet.hpp \
	Url.cpp \
	Url.hpp

libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
