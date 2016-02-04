#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([libhttp-server], [0.1], [bjtj10@gmail.com])

AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_LANG([C++])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_AWK
AC_PROG_MKDIR_P

# Checks for libraries.
ACX_PTHREAD
# AC_SEARCH_LIBS([OS::idle], [oslayer], [], [AC_MSG_ERROR([oslayer not found!!!])])
PKG_CHECK_MODULES([LIBOSLAYER], [oslayer])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday memset mkdir socket strcasecmp SSL_library_init])

if test $ac_cv_func_SSL_library_init = no; then
   AC_CHECK_LIB(ssl, SSL_library_init, [LIBS="-lssl -lcrypto $LIBS"
   AC_DEFINE(HAVE_OPENSSL, 1, [Define to 1 if you have the 'SSL_library_init' function.])])
fi

AC_CONFIG_FILES([Makefile
				 libhttp-server/Makefile
                 examples/Makefile
				 http-server.pc])

AC_OUTPUT
