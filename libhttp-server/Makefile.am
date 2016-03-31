libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = \
	AnotherHttpClient.hpp \
	AnotherHttpClientThreadPool.hpp \
	AnotherHttpServer.hpp \
	BufferIndicator.hpp \
	ChunkedReader.hpp \
	ChunkedTransfer.hpp \
	Communication.hpp \
	Connection.hpp \
	ConnectionManager.hpp \
	ConnectionThreadPool.hpp \
	DataSink.hpp \
	DataSource.hpp \
	DataTransfer.hpp \
	FileDataSource.hpp \
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
	HttpServerConfig.hpp \
	HttpSession.hpp \
	HttpSessionManager.hpp \
	HttpSessionTool.hpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	LispPage.hpp \
	Packet.hpp \
	StringDataSink.hpp \
	StringDataSource.hpp \
	Url.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	AnotherHttpClient.cpp \
	AnotherHttpClient.hpp \
	AnotherHttpClientThreadPool.cpp \
	AnotherHttpClientThreadPool.hpp \
	AnotherHttpServer.cpp \
	AnotherHttpServer.hpp \
	BufferIndicator.cpp \
	BufferIndicator.hpp \
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
	DataSink.hpp \
	DataSource.hpp \
	DataTransfer.cpp \
	DataTransfer.hpp \
	FileDataSource.hpp \
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
	HttpServerConfig.hpp \
	HttpSession.cpp \
	HttpSession.hpp \
	HttpSessionManager.cpp \
	HttpSessionManager.hpp \
	HttpSessionTool.hpp \
	HttpStatusCodes.cpp \
	HttpStatusCodes.hpp \
	InetAddress.hpp \
	LispPage.cpp \
	LispPage.hpp \
	Packet.cpp \
	Packet.hpp \
	StringDataSink.hpp \
	StringDataSource.hpp \
	Url.cpp \
	Url.hpp


libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
