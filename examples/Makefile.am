noinst_PROGRAMS=AnotherHttpClientTest AnotherHttpServerTest HttpClientTest FileBrowseHttpServerTest urlTest sessionTest

AnotherHttpClientTest_SOURCES = AnotherHttpClientTest.cpp
AnotherHttpClientTest_CPPFLAGS = -I$(top_srcdir)
AnotherHttpClientTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AnotherHttpClientTest_LDFLAGS=$(PTHREAD_CFLAGS)
AnotherHttpClientTest_LDADD = ../libhttp-server/libhttp-server.la


AnotherHttpServerTest_SOURCES = AnotherHttpServerTest.cpp
AnotherHttpServerTest_CPPFLAGS = -I$(top_srcdir)
AnotherHttpServerTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AnotherHttpServerTest_LDFLAGS=$(PTHREAD_CFLAGS)
AnotherHttpServerTest_LDADD = ../libhttp-server/libhttp-server.la

HttpClientTest_SOURCES = HttpClientTest.cpp
HttpClientTest_CPPFLAGS = -I$(top_srcdir)
HttpClientTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
HttpClientTest_LDFLAGS=$(PTHREAD_CFLAGS)
HttpClientTest_LDADD = ../libhttp-server/libhttp-server.la

FileBrowseHttpServerTest_SOURCES = AnotherHttpServerFileBrowseTest.cpp
FileBrowseHttpServerTest_CPPFLAGS = -I$(top_srcdir)
FileBrowseHttpServerTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
FileBrowseHttpServerTest_LDFLAGS=$(PTHREAD_CFLAGS)
FileBrowseHttpServerTest_LDADD = ../libhttp-server/libhttp-server.la

urlTest_SOURCES = urlTest.cpp
urlTest_CPPFLAGS = -I$(top_srcdir)
urlTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
urlTest_LDFLAGS=$(PTHREAD_CFLAGS)
urlTest_LDADD = ../libhttp-server/libhttp-server.la

sessionTest_SOURCES = SessionTest.cpp
sessionTest_CPPFLAGS = -I$(top_srcdir)
sessionTest_CXXFLAGS=$(PTHREAD_CXXFLAGS)
sessionTest_LDFLAGS=$(PTHREAD_CFLAGS)
sessionTest_LDADD = ../libhttp-server/libhttp-server.la
