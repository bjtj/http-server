bin_PROGRAMS=http-server
http_server_SOURCES= \
	os.hpp os.cpp \
	Text.hpp Text.cpp \
	Url.hpp Url.cpp \
	MultiConn.hpp MultiConn.cpp \
	HttpConnection.hpp \
	HttpHeader.hpp HttpHeader.cpp \
	HttpServer.hpp HttpServer.cpp \
	main.cpp

http_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)
http_server_LDFLAGS=$(PTHREAD_CFLAGS)
