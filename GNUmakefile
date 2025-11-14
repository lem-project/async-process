PREFIX ?= /usr/local

GENERATED_MAKEFILE := $(wildcard Makefile)

.PHONY: distclean
distclean:
	@if [ -f Makefile ]; then \
		echo "Running make distclean..."; \
		$(MAKE) -f Makefile distclean 2>/dev/null || true; \
	fi
	@echo "Removing autotools generated files..."
	@rm -rf Makefile Makefile.in configure config.* libtool aclocal.m4 stamp-h1 autom4te.cache
	@rm -rf compile config.guess config.sub depcomp install-sh ltmain.sh missing
	@rm -rf .libs .deps src/.libs src/.deps
	@rm -f *.lo *.la src/*.lo src/*.la
	@echo "All generated files removed. Run 'make' to rebuild."

# If Makefile exists, delegate to it
ifneq ($(GENERATED_MAKEFILE),)

.DEFAULT_GOAL := all

%:
	$(MAKE) -f Makefile $@

else

# No Makefile - need to generate build system
.DEFAULT_GOAL := all

configure: configure.ac Makefile.am
	@echo "Generating build system..."
	@which glibtoolize > /dev/null 2>&1 && glibtoolize --copy --force --quiet || libtoolize --copy --force --quiet
	@aclocal
	@autoheader
	@automake --add-missing --copy --foreign
	@autoconf
	@echo ""

Makefile: configure
	@echo "Running configure..."
	@./configure --prefix=$(PREFIX)
	@echo ""

.PHONY: all
all: Makefile
	@$(MAKE) -f Makefile all
	@echo ""
	@echo "Build complete. Install with: make install"
	@echo ""

.PHONY: build
build: all

.PHONY: install
install: Makefile
	@$(MAKE) -f Makefile install

.PHONY: clean
clean: Makefile
	@$(MAKE) -f Makefile clean

endif
