# GNUmakefile for async-process
# Handles autotools generation and delegates to generated Makefile

PREFIX ?= /usr/local

# Check if generated Makefile exists
GENERATED_MAKEFILE := $(wildcard Makefile)

# If Makefile exists, delegate to it
ifneq ($(GENERATED_MAKEFILE),)

%:
	$(MAKE) -f Makefile $@

else

# No Makefile - need to generate build system
.DEFAULT_GOAL := all

configure: configure.ac Makefile.am
	@echo "Generating build system..."
	@libtoolize --copy --force --quiet
	@aclocal -I m4
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
clean:
	@echo "Run 'make' first to generate build system"

endif
