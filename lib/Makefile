-include ../Makefile.conf

NO_CUSTOM = y
INSTALL_DLL = dlllacaml_stubs.so

SYSTEM=$(shell ocamlc -config 2>/dev/null|grep system|sed 's/system: //')

ifeq ($(shell uname), Darwin)
  BLAS =
  LAPACK =
  EXP10 = exp10.c
  LIBDIRS = /sw/lib
  OCAMLMKLIB_FLAGS = -framework vecLib
  OCAMLLDFLAGS = -ccopt "-framework vecLib"
else
ifeq ($(SYSTEM), mingw)
  # exp10 does not exists under cygwin-mingw
  EXP10 = exp10.c
  NO_CUSTOM =
  # For ocaml >= 3.11.0
  INSTALL_DLL = dlllacaml_stubs.dll
else
  EXP10 =
endif
endif

OCAMLMAKEFILE := ../OCamlMakefile

STUBS := \
	impl_c.c \
	utils_c.c \
	vec2_S_c.c vec2_D_c.c \
	vec2_C_c.c vec2_Z_c.c \
	mat4_S_c.c mat4_D_c.c \
	mat4_C_c.c mat4_Z_c.c \
	impl4_S_c.c impl4_D_c.c \
	impl4_C_c.c impl4_Z_c.c \
	impl2_S_c.c impl2_D_c.c \
	impl2_C_c.c impl2_Z_c.c $(EXP10)

%.c: lacaml_macros.h utils_c.h
utils_c.c: utils_c.h

SOURCES := \
	common.mli common.ml \
	utils.ml \
	float32.ml float64.ml \
	complex32.ml complex64.ml \
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
	impl4_S.mli impl4_S.ml \
	impl4_D.mli impl4_D.ml \
	impl4_C.mli impl4_C.ml \
	impl4_Z.mli impl4_Z.ml \
	impl2_S.mli impl2_S.ml \
	impl2_D.mli impl2_D.ml \
	impl2_C.mli impl2_C.ml \
	impl2_Z.mli impl2_Z.ml \
	io.mli \
	io.ml \
	impl.ml \
	$(STUBS)

OCAMLFLAGS := -w A -warn-error A -for-pack Lacaml
LIB_PACK_NAME := lacaml
#CFLAGS	:= -fPIC -DPIC -Wall -Werror -pedantic -Wno-unused -Wno-long-long
CFLAGS := -O2 -fPIC -DPIC
CLIBS := $(LAPACK) $(BLAS)
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
	impl2_S.mli impl2_S.ml impl2_D.mli impl2_D.ml \
	impl2_C.mli impl2_C.ml impl2_Z.mli impl2_Z.ml \
	impl4_S.mli impl4_S.ml impl4_D.mli impl4_D.ml \
	impl4_C.mli impl4_C.ml impl4_Z.mli impl4_Z.ml

LIBINSTALL_FILES := \
  io.mli impl.ml common.mli \
  impl_SDCZ.mli impl_SD.mli impl_CZ.mli \
  vec_SDCZ.mli vec_SD.mli vec_CZ.mli \
  mat_SDCZ.mli mat_SD.mli mat_CZ.mli \
  float32.ml float64.ml \
  complex32.ml complex64.ml \
  io.cmi utils.cmi common.cmi \
  lacaml.cmi lacaml.cma lacaml_top.cma lacaml.cmxa lacaml.a \
  liblacaml_stubs.a $(INSTALL_DLL)

TRASH := \
  $(PREMAKE) \
  install_printers.cmi install_printers.cmo \
  install_printers.annot lacaml_top.cma

#all:	bcl
all:

byte-code-library:	$(PREMAKE)
native-code-library:	$(PREMAKE)

toplevel-library:	byte-code-library \
			install_printers.cmi install_printers.cmo
	$(OCAMLC) -a -o lacaml_top.cma lacaml.cma install_printers.cmo

install:	libinstall
uninstall:	libuninstall

# Generation rules for precision-dependent C-code
%2_S_c.o:	%_SD_c.c
	$(CC) -c $(CFLAGS) $(CINCFLAGS) -I'$(OCAMLLIBPATH)' $< -o $@

%2_D_c.o:	%_SD_c.c
	$(CC) -c -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) -I'$(OCAMLLIBPATH)' \
		$< -o $@

%2_C_c.o:	%_CZ_c.c
	$(CC) -c -DLACAML_COMPLEX $(CFLAGS) $(CINCFLAGS) \
		-I'$(OCAMLLIBPATH)' $< -o $@

%2_Z_c.o:	%_CZ_c.c
	$(CC) -c -DLACAML_COMPLEX -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) \
		-I'$(OCAMLLIBPATH)' $< -o $@

%4_S_c.o:	%_SDCZ_c.c
	$(CC) -c $(CFLAGS) $(CINCFLAGS) -I'$(OCAMLLIBPATH)' $< -o $@

%4_D_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) -I'$(OCAMLLIBPATH)' \
		$< -o $@

%4_C_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_COMPLEX $(CFLAGS) $(CINCFLAGS) \
		-I'$(OCAMLLIBPATH)' $< -o $@

%4_Z_c.o:	%_SDCZ_c.c
	$(CC) -c -DLACAML_COMPLEX -DLACAML_DOUBLE $(CFLAGS) $(CINCFLAGS) \
		-I'$(OCAMLLIBPATH)' $< -o $@

# Generation rules for precision-dependent OCaml-code
%2_S.mli:	%_SD.mli
	sed -e "s/FPREC/S/g" \
	  -e "s/Floatxx/Float32/g" \
	  -e "s/floatxx/float32/g" $< > $@

%2_S.ml:		%_SD.ml
	sed -e "s/FPREC/S/g" \
	  -e "s/Floatxx/Float32/g" \
	  -e "s/floatxx/float32/g" $< > $@

%2_D.mli:	%_SD.mli
	sed -e "s/FPREC/D/g" \
	  -e "s/Floatxx/Float64/g" \
	  -e "s/floatxx/float64/g" $< > $@

%2_D.ml:		%_SD.ml
	sed -e "s/FPREC/D/g" \
	  -e "s/Floatxx/Float64/g" \
	  -e "s/floatxx/float64/g" $< > $@

%2_C.mli:	%_CZ.mli
	sed -e "s/CPREC/C/g" \
	  -e "s/Floatxx/Float32/g" \
	  -e "s/floatxx/float32/g" \
	  -e "s/CBPREC/S/g" \
	  -e "s/Complexxx/Complex32/g" \
	  -e "s/complexxx/complex32/g" $< > $@

%2_C.ml:		%_CZ.ml
	sed -e "s/CPREC/C/g" \
	  -e "s/Floatxx/Float32/g" \
	  -e "s/floatxx/float32/g" \
	  -e "s/CBPREC/S/g" \
	  -e "s/Complexxx/Complex32/g" \
	  -e "s/complexxx/complex32/g" $< > $@

%2_Z.mli:	%_CZ.mli
	sed -e "s/CPREC/Z/g" -e "s/CBPREC/D/g" \
	  -e "s/Floatxx/Float64/g" \
	  -e "s/floatxx/float64/g" \
	  -e "s/Complexxx/Complex64/g" \
	  -e "s/complexxx/complex64/g" $< > $@

%2_Z.ml:		%_CZ.ml
	sed -e "s/CPREC/Z/g" -e "s/CBPREC/D/g" \
	  -e "s/Floatxx/Float64/g" \
	  -e "s/floatxx/float64/g" \
	  -e "s/Complexxx/Complex64/g" \
	  -e "s/complexxx/complex64/g" $< > $@

%4_S.mli:	%_SDCZ.mli
	sed -e "s/NPREC/S/g" -e "s/NBPREC/S/g" \
	  -e "s/Numberxx/Float32/g" \
	  -e "s/numberxx/float32/g" $< > $@

%4_S.ml:		%_SDCZ.ml
	sed -e "s/NPREC/S/g" -e "s/NBPREC/S/g" \
	  -e "s/Numberxx/Float32/g" \
	  -e "s/numberxx/float32/g" $< > $@

%4_D.mli:	%_SDCZ.mli
	sed -e "s/NPREC/D/g" -e "s/NBPREC/D/g" \
	  -e "s/Numberxx/Float64/g" \
	  -e "s/numberxx/float64/g" $< > $@

%4_D.ml:		%_SDCZ.ml
	sed -e "s/NPREC/D/g" -e "s/NBPREC/D/g" \
	  -e "s/Numberxx/Float64/g" \
	  -e "s/numberxx/float64/g" $< > $@

%4_C.mli:	%_SDCZ.mli
	sed -e "s/NPREC/C/g" -e "s/NBPREC/S/g" \
	  -e "s/Numberxx/Complex32/g" \
	  -e "s/numberxx/complex32/g" $< > $@

%4_C.ml:		%_SDCZ.ml
	sed -e "s/NPREC/C/g" -e "s/NBPREC/S/g" \
	  -e "s/Numberxx/Complex32/g" \
	  -e "s/numberxx/complex32/g" $< > $@

%4_Z.mli:	%_SDCZ.mli
	sed -e "s/NPREC/Z/g" -e "s/NBPREC/D/g" \
	  -e "s/Numberxx/Complex64/g" \
	  -e "s/numberxx/complex64/g" $< > $@

%4_Z.ml:		%_SDCZ.ml
	sed -e "s/NPREC/Z/g" -e "s/NBPREC/D/g" \
	  -e "s/Numberxx/Complex64/g" \
	  -e "s/numberxx/complex64/g" $< > $@

clean::	clean-doc

-include $(OCAMLMAKEFILE)
