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
	@make clean-examples
	@$(RM) -f doc
	-touch setup.ml # force reconfigure

EXAMPLES = $(filter-out examples/OMakefile examples/Makefile.examples, $(wildcard examples/*))

all-old:
	@cd lib && $(MAKE) byte-code-library native-code-library toplevel-library

.PHONY:	examples
examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE)); done

htdoc:
	@cd lib && $(MAKE) $@
	ln -sf lib/doc


.PHONY: clean-examples
clean-examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE) clean); done
