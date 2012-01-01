define PROJ_lin_eq
  SOURCES = lin_eq.ml
  RESULT = lin_eq
endef
export PROJ_lin_eq

define PROJ_gbsv
  SOURCES = gbsv.ml
  RESULT = gbsv
endef
export PROJ_gbsv

ifndef SUBPROJS
  export SUBPROJS = lin_eq gbsv
endif

all: bc

%:
	@make -f ../Makefile.examples subprojs SUBTARGET=$@
