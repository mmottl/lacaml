TARGETS = $(addsuffix .bc, \
  nag_gbsv nag_gesv nag_gtsv nag_pbsv nag_posv \
	nag_ppsv nag_ptsv nag_spsv nag_sysv nag_gbsv)

.PHONY: all clean

all:
	@echo jbuilder: Entering directory \`$(abspath $(dir $(lastword $(PWD))))\'
	@jbuilder build --dev $(TARGETS)

clean:
	@jbuilder clean
