#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([libhttp-server], [0.1], [bjtj10@gmail.com])

AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

AM_SILENT_RULES([yes])

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
PKG_CHECK_MODULES([LIBOSLAYER], [oslayer])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h sys/socket.h sys/time.h unistd.h])

# Debugging
AC_ARG_ENABLE(debug,
        AS_HELP_STRING([--enable-debug], [enable debugging]),,
        enable_debug=no)
if test "x$enable_debug" = "xyes"; then
        CFLAGS="$CFLAGS -g -Wall"
		CXXFLAGS="$CXXFLAGS -g -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_CHECK_FUNCS([gettimeofday memset mkdir socket strcasecmp SSL_library_init])

PKG_CHECK_MODULES([LIBOPENSSL],[openssl],[HAVE_PKG_OPENSSL=yes],[HAVE_PKG_OPENSSL=no])
AS_IF([test "x$HAVE_PKG_OPENSSL" = xyes], [AC_DEFINE([HAVE_OPENSSL], 1, [support openssl])])

AC_CONFIG_FILES([Makefile
				 libhttp-server/Makefile
                 examples/Makefile
				 tests/Makefile
				 http-server.pc])

AC_OUTPUT
echo \
OpenSSL enabled : ${HAVE_PKG_OPENSSL}
