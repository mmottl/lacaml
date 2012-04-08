WEB = lacaml.forge.ocamlcore.org:/home/groups/lacaml/htdocs/

DIR = $(shell oasis query name)-$(shell oasis query version)
TARBALL = $(DIR).tar.gz

DISTFILES = API.odocl AUTHORS.txt Changelog COPYRIGHT.txt \
  INSTALL.txt INSTALL.win32 LICENSE.txt README.txt \
  Makefile make_prec_dep.ml myocamlbuild.ml _oasis setup.ml setup.conf _tags \
  $(wildcard lib/*) $(wildcard examples/*)

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml make_prec_dep.ml lib/*_SDCZ.mli lib/*_SD.mli lib/*_CZ.mli
	ocaml setup.ml -configure

setup.ml API.odocl: _oasis
	oasis setup

doc install uninstall reinstall: all
	ocaml setup.ml -$@

doc: API.odocl

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

.PHONY: dist tar
dist tar: setup.ml
	mkdir -p $(DIR)
	for f in $(DISTFILES); do \
	  cp -r --parents $$f $(DIR); \
	done
	tar -zcvf $(TARBALL) --exclude='*~' $(DIR)
	$(RM) -r $(DIR)

.PHONY: debian
debian:
	oasis2debian init --debian-name $(shell oasis query name)

.PHONY:	clean
clean: setup.ml
	ocaml setup.ml -clean
	$(RM) $(wildcard $(TARBALL))
	-touch setup.ml # force reconfigure

distclean: setup.ml
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
