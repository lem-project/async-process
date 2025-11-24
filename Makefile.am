lib_LTLIBRARIES = libasyncprocess.la
libasyncprocess_la_LDFLAGS = -version-info @LT_VERSION_INFO@ -no-undefined
libasyncprocess_la_SOURCES = src/async-process.c

include_HEADERS = src/async-process.h

install-exec-hook:
	rm -f $(DESTDIR)$(libdir)/libasyncprocess.la
