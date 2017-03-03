noinst_PROGRAMS=HttpServerSample ChatServer

HttpServerSample_SOURCES=HttpServerSample.cpp
ChatServer_SOURCES=ChatServer.cpp

AM_CPPFLAGS = -I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS) -DDATA_PATH="\"$(srcdir)/data\""
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD = ../libhttp-server/libhttp-server.la

EXTRA_DIST=data/mimetypes
