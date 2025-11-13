# GNUmakefile for async-process
# This Makefile handles the complete build process including autotools setup

# Default installation prefix
PREFIX ?= /usr/local

# Check if we have the generated Makefile
GENERATED_MAKEFILE := $(wildcard Makefile)

# If Makefile exists, just delegate everything to it
ifneq ($(GENERATED_MAKEFILE),)

# Makefile exists - delegate all targets to it
%:
	$(MAKE) -f Makefile $@

else

# No Makefile yet - we need to generate it
.DEFAULT_GOAL := all

# Check if configure exists
HAVE_CONFIGURE := $(wildcard configure)

# Generate configure script and Makefile.in from autotools sources
.PHONY: autogen
autogen:
	@echo "Generating build system with autotools..."
	@libtoolize --copy --force
	@aclocal -I m4
	@autoheader
	@automake --add-missing --copy --foreign
	@autoconf
	@echo "Autotools generation complete."
	@echo ""

# Generate configure if needed
configure: configure.ac Makefile.am
ifeq ($(HAVE_CONFIGURE),)
	@$(MAKE) -f GNUmakefile autogen
else
	@echo "configure already exists"
endif

# Run configure to generate Makefile
Makefile: configure
	@echo "Running configure with prefix=$(PREFIX)..."
	@./configure --prefix=$(PREFIX)
	@echo ""

# Default target - generate Makefile then build
.PHONY: all
all: Makefile
	@$(MAKE) -f Makefile all
	@echo ""
	@echo "Build complete! Install with:"
	@echo "  make install PREFIX=$(PREFIX)"
	@echo ""

# Install target
.PHONY: install
install: Makefile
	@$(MAKE) -f Makefile install

# Help when no Makefile exists
.PHONY: help
help:
	@echo "async-process Build System"
	@echo ""
	@echo "No Makefile found. Available targets:"
	@echo "  make              - Generate build system and build the library"
	@echo "  make autogen      - Generate configure script from autotools"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo "After running 'make', you can use:"
	@echo "  make install PREFIX=/path  - Install the library"
	@echo "  make clean                 - Clean build artifacts"
	@echo "  make distclean             - Clean all generated files"
	@echo ""

endif

# Maintainer targets that work regardless of Makefile existence
.PHONY: maintainer-clean realclean
maintainer-clean realclean:
	@if [ -f Makefile ]; then $(MAKE) -f Makefile distclean 2>/dev/null || true; fi
	@echo "Removing all generated files..."
	@rm -f config.status config.log config.h config.h.in config.h.in~ stamp-h1
	@rm -f configure aclocal.m4
	@rm -f Makefile.in Makefile
	@rm -f compile config.guess config.sub depcomp install-sh missing
	@rm -f ltmain.sh libtool config.lt
	@rm -rf autom4te.cache .libs _libs
	@rm -f m4/libtool.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4 m4/lt~obsolete.m4
	@echo "Cleaned. Run 'make' to rebuild from scratch."
