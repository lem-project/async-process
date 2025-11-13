ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libasyncprocess.la
libasyncprocess_la_LDFLAGS = -version-info 0:1:0 -no-undefined
libasyncprocess_la_SOURCES = src/async-process.c

include_HEADERS = src/async-process.h

# Don't install .la files
install-exec-hook:
	rm -f $(DESTDIR)$(libdir)/libasyncprocess.la
