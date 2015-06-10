bin_PROGRAMS=http-server
http_server_SOURCES= \
	Text.hpp Text.cpp \
	HttpHeader.hpp HttpHeader.cpp \
	HttpConnection.hpp \
	HttpServer.hpp HttpServer.cpp \
	Url.hpp Url.cpp \
	os.hpp os.cpp main.cpp

http_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)
http_server_LDFLAGS=$(PTHREAD_CFLAGS)
