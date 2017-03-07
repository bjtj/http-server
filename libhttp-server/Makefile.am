libhttp_serverincdir=${includedir}/libhttp-server
libhttp_serverinc_HEADERS = \
	AnotherHttpClient.hpp \
	AnotherHttpServer.hpp \
	BasicAuth.hpp \
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
	HttpAuth.hpp \
	HttpEncoderDecoder.hpp \
	HttpException.hpp \
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
	LispPage.hpp \
	MimeTypes.hpp \
	Packet.hpp \
	StringDataSink.hpp \
	StringDataSource.hpp \
	Url.hpp \
	WebServerUtil.hpp

lib_LTLIBRARIES = libhttp-server.la
libhttp_server_la_SOURCES = \
	AnotherHttpClient.cpp \
	AnotherHttpClient.hpp \
	AnotherHttpServer.cpp \
	AnotherHttpServer.hpp \
	BasicAuth.cpp \
	BasicAuth.hpp \
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
	HttpAuth.cpp \
	HttpAuth.hpp \
	HttpEncoderDecoder.cpp \
	HttpEncoderDecoder.hpp \
	HttpException.hpp \
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
	LispPage.cpp \
	LispPage.hpp \
	MimeTypes.cpp \
	MimeTypes.hpp \
	Packet.cpp \
	Packet.hpp \
	StringDataSink.hpp \
	StringDataSource.hpp \
	Url.cpp \
	Url.hpp \
	WebServerUtil.cpp \
	WebServerUtil.hpp

libhttp_server_la_CXXFLAGS = $(PTHREAD_CXXFLAGS) $(LIBOSLAYER_CFLAGS)
libhttp_server_la_LIBADD = $(LIBOSLAYER_LIBS)
