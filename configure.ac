AC_INIT([async-process], [0.1])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])

# Set default prefix based on platform if not specified
AC_PREFIX_DEFAULT([/usr/local])

AM_INIT_AUTOMAKE([foreign subdir-objects])
LT_INIT([shared win32-dll disable-static])
AC_PROG_CC

# Platform-specific configuration
AC_CANONICAL_HOST
case $host_os in
  freebsd*)
    # FreeBSD typically uses /usr/local
    test "$prefix" = NONE && prefix=/usr/local
    ;;
  darwin*)
    # macOS typically uses /usr/local
    test "$prefix" = NONE && prefix=/usr/local
    ;;
  linux*)
    # Linux typically uses /usr/local for user-installed software
    test "$prefix" = NONE && prefix=/usr/local
    ;;
esac

AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h windows.h])

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

  You can now run 'make' to build the library.
  After building, run 'make install' to install to the prefix.

  To install to a different location, reconfigure with:
    ./configure --prefix=/your/custom/path
])
