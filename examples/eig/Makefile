define PROJ_eig
  SOURCES = eig.ml
  RESULT = eig
endef
export PROJ_eig

define PROJ_sbgv
  SOURCES = sbgv.ml
  RESULT = sbgv
endef
export PROJ_sbgv

ifndef SUBPROJS
  export SUBPROJS = eig sbgv
endif

all: bc

%:
	@make -f ../Makefile.examples subprojs SUBTARGET=$@

