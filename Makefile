-include Makefile.conf

.PHONY: configure all byte native doc upload-doc install uninstall reinstall
all byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

doc install uninstall reinstall: all
	ocaml setup.ml -$@

.PHONY:	clean
clean:
	-ocaml setup.ml -clean
	@$(MAKE) -C lib clean
	@$(MAKE) -C examples clean-examples
	@$(RM) -f doc
	-touch setup.ml # force reconfigure
