EXAMPLES = $(filter-out OMakefile Makefile.examples, $(wildcard *))

.PHONY:	examples
examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE)); done


.PHONY: clean-examples
clean-examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE) clean); done
