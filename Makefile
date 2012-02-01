WEB = lacaml.forge.ocamlcore.org:/home/groups/lacaml/htdocs/

DIR = $(shell oasis query name)-$(shell oasis query version)
TARBALL = $(DIR).tar.gz

DISTFILES = API.odocl AUTHORS.txt Changelog COPYRIGHT.txt \
  INSTALL.txt INSTALL.win32 LICENSE.txt README.txt \
  Makefile make_prec_dep.ml myocamlbuild.ml _oasis setup.ml _tags \
  $(wildcard lib/*) $(wildcard examples/*)

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml make_prec_dep.ml lib/lacaml_SDCZ.mli
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

doc install uninstall reinstall: all
	ocaml setup.ml -$@

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

.PHONY:	clean
clean: setup.ml
	ocaml setup.ml -clean
	$(RM) $(TARBALL)
	-touch setup.ml # force reconfigure

distclean: setup.ml
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
