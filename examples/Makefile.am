noinst_PROGRAMS=HttpServerSample

HttpServerSample_SOURCES=HttpServerSample.cpp

AM_CPPFLAGS = -I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS) -DDATA_PATH="\"$(srcdir)/data\""
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD = ../libhttp-server/libhttp-server.la

EXTRA_DIST=data/mimetypes
