noinst_PROGRAMS=AnotherHttpServerTest HttpClientTest FileBrowseHttpServerTest

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

