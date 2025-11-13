ACLOCAL_AMFLAGS = -I m4

# Install library as a shared library instead of a module
lib_LTLIBRARIES = libasyncprocess.la
libasyncprocess_la_LDFLAGS = -version-info 0:1:0 -no-undefined
libasyncprocess_la_SOURCES = src/async-process.c

# Install the header file
include_HEADERS = src/async-process.h

# Optional: development copy target for building static directory
copy:
	@OS=$$(uname); \
	ARCH=$$(uname -m); \
	$(MKDIR_P) static/$$ARCH/$$OS; \
	if [ "$$OS" = "Darwin" ]; then \
	  cp -f .libs/libasyncprocess.*.dylib static/$$ARCH/$$OS/libasyncprocess.dylib 2>/dev/null || \
	  cp -f .libs/libasyncprocess.dylib static/$$ARCH/$$OS/libasyncprocess.dylib; \
	elif [ -n "$(MSYSTEM)" ]; then \
	  if [ "$(MSYSTEM)" = "MINGW32" ]; then \
	    $(MKDIR_P) static/x86/windows; \
	    cp -f .libs/libasyncprocess.dll static/x86/windows/libasyncprocess.dll; \
	  else \
	    $(MKDIR_P) static/$$ARCH/windows; \
	    cp -f .libs/libasyncprocess.dll static/$$ARCH/windows/libasyncprocess.dll; \
	  fi; \
	else \
	  cp -f .libs/libasyncprocess.so* static/$$ARCH/$$OS/libasyncprocess.so 2>/dev/null || \
	  cp -f .libs/libasyncprocess.so static/$$ARCH/$$OS/libasyncprocess.so; \
	fi

.PHONY: copy
