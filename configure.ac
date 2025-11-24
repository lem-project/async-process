AC_INIT([async-process], [0.1])
AC_CONFIG_HEADERS([config.h])

# Library versioning (current:revision:age)
AC_SUBST([LT_VERSION_INFO], [0:0:0])

# Set default prefix based on platform if not specified
AC_PREFIX_DEFAULT([/usr/local])

AM_INIT_AUTOMAKE([foreign subdir-objects])

# Must detect C compiler before initializing libtool
AC_PROG_CC

# Initialize libtool (disable static by default, enable with --enable-static)
LT_INIT([disable-static])

AC_CANONICAL_HOST
test "$prefix" = NONE && prefix=/usr/local

AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])

AC_CHECK_HEADER_STDBOOL
AC_TYPE_PID_T

AC_FUNC_FORK
AC_FUNC_MALLOC
AC_CHECK_FUNCS([dup2 strerror])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# Print configuration summary
AC_MSG_NOTICE([
  async-process configuration summary:

  Installation prefix: $prefix
  Library directory:   $libdir
  Include directory:   $includedir
  C Compiler:          $CC
  Host system:         $host_os
  Build shared:        $enable_shared
  Build static:        $enable_static

  Note: This C library is for Unix-like systems only (Linux, FreeBSD, macOS).
  Windows support is provided via pure Lisp CFFI implementation and does not
  require C compilation.

  You can now run 'make' to build the library.
  After building, run 'make install' to install to the prefix.

  To install to a different location, reconfigure with:
    ./configure --prefix=/your/custom/path

  To build static library:
    ./configure --enable-static

  To build only static (no shared):
    ./configure --enable-static --disable-shared
])
