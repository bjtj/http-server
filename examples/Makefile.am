noinst_PROGRAMS=FileBrowseHttpServerTest

FileBrowseHttpServerTest_SOURCES=AnotherHttpServerFileBrowseTest.cpp

AM_CPPFLAGS = -I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD = ../libhttp-server/libhttp-server.la
