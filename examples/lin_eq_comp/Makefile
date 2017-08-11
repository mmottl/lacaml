TARGETS = $(addsuffix .bc, lin_eq_comp)

.PHONY: all clean

all:
	@echo jbuilder: Entering directory \`$(abspath $(dir $(lastword $(PWD))))\'
	@jbuilder build --dev $(TARGETS)

clean:
	@jbuilder clean
