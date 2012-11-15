# This Makefile will build all OCaml code, for development.
# Sub-projects may have their own Makefiles internally to build
# non-ocaml files and targets.

include Makefile.config

OCPBUILD=./ocp-build-boot/ocp-build
OCPBUILD_FLAGS=

all: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS) -scan
init: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS) -init -scan
verbose: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS) -v 5
byte: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS) -byte
opt: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS) -asm
noscan: $(OCPBUILD)
	$(OCPBUILD) $(OCPBUILD_FLAGS)

ocp-build-boot/ocp-build.boot: boot/ocp-build.boot
	cp -f boot/ocp-build.boot ocp-build-boot/ocp-build.boot

WIN32_FILES= \
  libs/stubs/win32/win32_waitpids_c.c \
  libs/stubs/win32/win32_fileinfo_c.c

ocp-build-boot/win32_c.c: $(WIN32_FILES)
	cat $(WIN32_FILES) > ocp-build-boot/win32_c.c

ocp-build-boot/ocp-build: ocp-build-boot/ocp-build.boot ocp-build-boot/win32_c.c
	$(MAKE) -C ocp-build-boot

scan: $(OCPBUILD)
	$(OCPBUILD) -scan

sanitize: $(OCPBUILD)
	$(OCPBUILD) -sanitize

ocpbuild: $(OCPBUILD)
	$(OCPBUILD) ocp-build

clean: $(OCPBUILD)
	$(OCPBUILD) -clean

distclean: clean $(OCPBUILD)
	$(OCPBUILD) -distclean
	$(MAKE) -C ocp-build-boot clean

install:
	mkdir -p $(OCPBUILDDIR)
	mkdir -p $(BINDIR)
	cp _obuild/ocp-build/ocp-build.asm $(BINDIR)/ocp-build

uninstall:
	rm -f $(BINDIR)/ocp-build

old-ocp-build:
	OCAML_VERSION=ocaml-3.12.1 ocp-build -arch 3.12.1 ocp-build

bootstrap: old-ocp-build
	rm -rf Saved
	mv boot Saved
	mkdir boot
	mv Saved boot/Saved
	ocp-bytecode _obuild/3.12.1/ocp-build/ocp-build.byte \
	   -make-static \
	   -filter-unused-prims \
	   -remove-prims tools/ocp-build/remove-primitives.txt \
	   -o boot/ocp-build.boot
	$(MAKE) clean
	$(MAKE) byte

# restore saved version of boot/
restore:
	mv boot/Saved Current
	mv boot boot-to-remove
	mv Current boot
	rm -rf boot-to-remove

# clean all old versions of boot/./
bootclean:
	rm -rf boot/Saved

make-boot:
	ocamlc -o boot/ocp-build.boot -use-runtime boot/ocp-build-runner \
	   -use-prims boot/prims_needed.txt \
	   unix.cma \
           ./_obuild/ocplib-lang/ocplib-lang.cma \
           ./_obuild/ocplib-system/ocplib-system.cma \
           ./_obuild/ocp-build-engine/ocp-build-engine.cma \
           ./_obuild/ocp-build-lib/ocp-build-lib.cma \
           ./_obuild/ocp-build/buildMain.cmo

doc:
	cd docs/user-manual; $(MAKE)



configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
