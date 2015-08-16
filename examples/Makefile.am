noinst_PROGRAMS=web-server http-server

# web server
web_server_SOURCES= \
	HttpRequestHandlers.hpp HttpRequestHandlers.cpp \
	WebServer.cpp
web_server_CPPFLAGS = -I$(top_srcdir)/libhttp-server
web_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)
web_server_LDFLAGS=$(PTHREAD_CFLAGS)
web_server_LDADD = ../libhttp-server/libhttp-server.la

# http server
http_server_SOURCES = HttpServer.cpp
http_server_CPPFLAGS = -I$(top_srcdir)/libhttp-server
http_server_CXXFLAGS=$(PTHREAD_CXXFLAGS)
http_server_LDFLAGS=$(PTHREAD_CFLAGS)
http_server_LDADD = ../libhttp-server/libhttp-server.la
