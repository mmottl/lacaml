define PROJ_eig
  SOURCES = eig.ml
  RESULT = eig
endef
export PROJ_eig

define PROJ_sbev
  SOURCES = sbev.ml
  RESULT = sbev
endef
export PROJ_sbev

define PROJ_sbgv
  SOURCES = sbgv.ml
  RESULT = sbgv
endef
export PROJ_sbgv

ifndef SUBPROJS
  export SUBPROJS = eig sbev sbgv
endif

all: bc

%:
	@make -f ../Makefile.examples subprojs SUBTARGET=$@

