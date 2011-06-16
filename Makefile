-include Makefile.conf

EXAMPLES = $(filter-out examples/OMakefile examples/Makefile.examples, $(wildcard examples/*))

.PHONY: all
all:
	@cd lib && $(MAKE) byte-code-library native-code-library toplevel-library

.PHONY:	examples
examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE)); done

.PHONY:	doc
doc htdoc:
	@cd lib && $(MAKE) $@
	ln -sf lib/doc

.PHONY:	install
install:
	@cd lib && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY:	clean
clean:
	@cd lib && $(MAKE) clean
	@make clean-examples
	@rm -f doc

.PHONY: clean-examples
clean-examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE) clean); done
