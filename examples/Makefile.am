noinst_PROGRAMS=HttpServerSample ChatServer

HttpServerSample_SOURCES=HttpServerSample.cpp
ChatServer_SOURCES=ChatServer.cpp

AM_CPPFLAGS = -I$(top_srcdir)

# cflags
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_CXXFLAGS+=-DDATA_PATH="\"$(srcdir)/data\""
AM_CXXFLAGS+=$(LIBOPENSSL_CFLAGS)

# libs
AM_LDFLAGS=$(PTHREAD_CFLAGS)
AM_LDFLAGS+=$(LIBOPENSSL_LIBS)

LDADD = ../libhttp-server/libhttp-server.la

EXTRA_DIST=data/mimetypes
