-include ../Makefile.conf

ifeq ($(shell uname),Darwin)
  BLAS =
  LAPACK =
  F2C =
  STATIC = y
  NO_CUSTOM =
  EXP10 = exp10.c
  LIBDIRS = /sw/lib
  OCAMLMKLIB_FLAGS = -framework vecLib
  OCAMLBLDFLAGS = -ccopt "-framework vecLib"
  INSTALL_DLL =
else
  EXP10 =
  NO_CUSTOM = y
  INSTALL_DLL = dlllacaml_stubs.so
endif

OCAMLMAKEFILE := ../OCamlMakefile

STUBS := \
	lacaml_c.c \
	lacaml_utils_c.c \
	vec2_S_c.c vec2_D_c.c \
	vec2_C_c.c vec2_Z_c.c \
	mat2_S_c.c mat2_D_c.c \
	mat2_C_c.c mat2_Z_c.c \
	lacaml4_S_c.c lacaml4_D_c.c \
	lacaml4_C_c.c lacaml4_Z_c.c \
	lacaml2_S_c.c lacaml2_D_c.c \
	lacaml2_C_c.c lacaml2_Z_c.c $(EXP10)

%.c: lacaml_macros.h lacaml_utils_c.h
lacaml_utils_c.c: lacaml_utils_c.h

SOURCES := \
	lacaml_common.mli lacaml_common.ml \
	lacaml_utils.ml \
	lacaml_float32.ml lacaml_float64.ml \
	lacaml_complex32.ml lacaml_complex64.ml \
	vec4_S.mli vec4_S.ml \
	vec4_D.mli vec4_D.ml \
	vec4_C.mli vec4_C.ml \
	vec4_Z.mli vec4_Z.ml \
	vec2_S.mli vec2_S.ml \
	vec2_D.mli vec2_D.ml \
	vec2_C.mli vec2_C.ml \
	vec2_Z.mli vec2_Z.ml \
	mat4_S.mli mat4_S.ml \
	mat4_D.mli mat4_D.ml \
	mat4_C.mli mat4_C.ml \
	mat4_Z.mli mat4_Z.ml \
	mat2_S.mli mat2_S.ml \
	mat2_D.mli mat2_D.ml \
	mat2_C.mli mat2_C.ml \
	mat2_Z.mli mat2_Z.ml \
	lacaml4_S.mli lacaml4_S.ml \
	lacaml4_D.mli lacaml4_D.ml \
	lacaml4_C.mli lacaml4_C.ml \
	lacaml4_Z.mli lacaml4_Z.ml \
	lacaml2_S.mli lacaml2_S.ml \
	lacaml2_D.mli lacaml2_D.ml \
	lacaml2_C.mli lacaml2_C.ml \
	lacaml2_Z.mli lacaml2_Z.ml \
	lacaml_io.mli \
	lacaml_io.ml \
	lacaml.ml \
	$(STUBS)

OCAMLFLAGS := -warn-error A
#CFLAGS	:= -fPIC -DPIC -Wall -Werror -pedantic -Wno-unused -Wno-long-long
CFLAGS := -O2 -fPIC -DPIC
CLIBS := $(LAPACK) $(BLAS) $(F2C)
RESULT := lacaml

PREMAKE := \
	$(STUBS:%.c=%.o) \
	vec4_S.mli vec4_S.ml vec4_D.mli vec4_D.ml \
	vec4_C.mli vec4_C.ml vec4_Z.mli vec4_Z.ml \
	vec2_S.mli vec2_S.ml vec2_D.mli vec2_D.ml \
	vec2_C.mli vec2_C.ml vec2_Z.mli vec2_Z.ml \
	mat4_S.mli mat4_S.ml mat4_D.mli mat4_D.ml \
	mat4_C.mli mat4_C.ml mat4_Z.mli mat4_Z.ml \
	mat2_S.mli mat2_S.ml mat2_D.mli mat2_D.ml \
	mat2_C.mli mat2_C.ml mat2_Z.mli mat2_Z.ml \
	lacaml2_S.mli lacaml2_S.ml lacaml2_D.mli lacaml2_D.ml \
	lacaml2_C.mli lacaml2_C.ml lacaml2_Z.mli lacaml2_Z.ml \
	lacaml4_S.mli lacaml4_S.ml lacaml4_D.mli lacaml4_D.ml \
	lacaml4_C.mli lacaml4_C.ml lacaml4_Z.mli lacaml4_Z.ml

LIBINSTALL_FILES := \
  lacaml_io.mli lacaml.ml lacaml_common.mli \
  lacaml_SDCZ.mli lacaml_SD.mli lacaml_CZ.mli \
  vec_SDCZ.mli vec_SD.mli vec_CZ.mli \
  mat_SDCZ.mli mat_SD.mli mat_CZ.mli \
  lacaml_float32.ml lacaml_float32.cmi \
  lacaml_float64.ml lacaml_float64.cmi \
  lacaml_complex32.ml lacaml_complex32.cmi \
  lacaml_complex64.ml lacaml_complex64.cmi \
  lacaml_common.cmi lacaml_io.cmi \
  lacaml_install_printers.cmi \
  lacaml.cmi lacaml.cma lacaml_top.cma lacaml.cmxa lacaml.a \
  liblacaml_stubs.a $(INSTALL_DLL)

TRASH := \
  $(PREMAKE) \
  lacaml_install_printers.cmi lacaml_install_printers.cmo \
  lacaml_install_printers.annot lacaml_top.cma

#all:	bcl
all:

byte-code-library:	$(PREMAKE)
native-code-library:	$(PREMAKE)

toplevel-library:	byte-code-library \
			lacaml_install_printers.cmi lacaml_install_printers.cmo
	$(OCAMLC) -a -o lacaml_top.cma lacaml.cma lacaml_install_printers.cmo

install:	libinstall
uninstall:	libuninstall

# Generation rules for precision-dependent C-code
%2_S_c.o:	%_SD_c.c
	$(CC) -c $(CFLAGS) $(CINCFLAGS) -I$(OCAMLLIBPATH) $< -o $@

%2_D_c.o:	%_SD_c.c
	$(CC) -c -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) -I$(OCAMLLIBPATH) \
		$< -o $@

%2_C_c.o:	%_CZ_c.c
	$(CC) -c -DLACAML_COMPLEX $(CFLAGS) $(CINCFLAGS) \
		-I$(OCAMLLIBPATH) $< -o $@

%2_Z_c.o:	%_CZ_c.c
	$(CC) -c -DLACAML_COMPLEX -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) \
		-I$(OCAMLLIBPATH) $< -o $@

%4_S_c.o:	%_SDCZ_c.c
	$(CC) -c $(CFLAGS) $(CINCFLAGS) -I$(OCAMLLIBPATH) $< -o $@

%4_D_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) -I$(OCAMLLIBPATH) \
		$< -o $@

%4_C_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_COMPLEX $(CFLAGS) $(CINCFLAGS) \
		-I$(OCAMLLIBPATH) $< -o $@

%4_Z_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_COMPLEX -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) \
		-I$(OCAMLLIBPATH) $< -o $@

# Generation rules for precision-dependent OCaml-code
%2_S.mli:	%_SD.mli
	sed -e "s/FPREC/S/g" -e "s/floatxx/float32/g" $< > $@

%2_S.ml:		%_SD.ml
	sed -e "s/FPREC/S/g" -e "s/floatxx/float32/g" $< > $@

%2_D.mli:	%_SD.mli
	sed -e "s/FPREC/D/g" -e "s/floatxx/float64/g" $< > $@

%2_D.ml:		%_SD.ml
	sed -e "s/FPREC/D/g" -e "s/floatxx/float64/g" $< > $@

%2_C.mli:	%_CZ.mli
	sed -e "s/CPREC/C/g" -e "s/floatxx/float32/g" \
	    -e "s/CBPREC/S/g" -e "s/complexxx/complex32/g" $< > $@

%2_C.ml:		%_CZ.ml
	sed -e "s/CPREC/C/g" -e "s/floatxx/float32/g" \
	    -e "s/CBPREC/S/g" -e "s/complexxx/complex32/g" $< > $@

%2_Z.mli:	%_CZ.mli
	sed -e "s/CPREC/Z/g" -e "s/CBPREC/D/g" -e "s/floatxx/float64/g" \
	    -e "s/complexxx/complex64/g" $< > $@

%2_Z.ml:		%_CZ.ml
	sed -e "s/CPREC/Z/g" -e "s/CBPREC/D/g" -e "s/floatxx/float64/g" \
	    -e "s/complexxx/complex64/g" $< > $@

%4_S.mli:	%_SDCZ.mli
	sed -e "s/NPREC/S/g" -e "s/NBPREC/S/g" -e "s/numberxx/float32/g" $< > $@

%4_S.ml:		%_SDCZ.ml
	sed -e "s/NPREC/S/g" -e "s/NBPREC/S/g" -e "s/numberxx/float32/g" $< > $@

%4_D.mli:	%_SDCZ.mli
	sed -e "s/NPREC/D/g" -e "s/NBPREC/D/g" -e "s/numberxx/float64/g" $< > $@

%4_D.ml:		%_SDCZ.ml
	sed -e "s/NPREC/D/g" -e "s/NBPREC/D/g" -e "s/numberxx/float64/g" $< > $@

%4_C.mli:	%_SDCZ.mli
	sed -e "s/NPREC/C/g" -e "s/NBPREC/S/g" -e "s/numberxx/complex32/g" $< > $@

%4_C.ml:		%_SDCZ.ml
	sed -e "s/NPREC/C/g" -e "s/NBPREC/S/g" -e "s/numberxx/complex32/g" $< > $@

%4_Z.mli:	%_SDCZ.mli
	sed -e "s/NPREC/Z/g" -e "s/NBPREC/D/g" -e "s/numberxx/complex64/g" $< > $@

%4_Z.ml:		%_SDCZ.ml
	sed -e "s/NPREC/Z/g" -e "s/NBPREC/D/g" -e "s/numberxx/complex64/g" $< > $@

clean::	clean-doc

-include $(OCAMLMAKEFILE)
