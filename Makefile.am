pkglib_LTLIBRARIES = libasyncprocess.la
libasyncprocess_la_LDFLAGS = -module -avoid-version
libasyncprocess_la_SOURCES = async-process.c

copy:
	cp -f .libs/libasyncprocess.so static/libasyncprocess-`uname -m`-`uname`.so|true
	cp -f .libs/libasyncprocess.dylib static/libasyncprocess-`uname -m`-`uname`.dylib|true
	cp -f .libs/libasyncprocess.dll static/libasyncprocess-`uname -m`-`uname`.dll|true
