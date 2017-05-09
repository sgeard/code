.PHONY: all clean help archive

all: libcode.a libcode.so archive

SRC := $(wildcard  *.f90)
OBJ := ${SRC:%.f90=%.o}

ifdef intel
F90 := ifort
F90_OPTS := -c -fPIC -fpp -O3
LINK_OPTS := -static-intel
ARCH_NAME := build-intel.tgz
else
F90 := gfortran
F90_OPTS := -c -fPIC -cpp -O3
LINK_OPTS :=
ARCH_NAME := build.tgz
endif

autodiff.o: autodiff.f90
	$(F90) $(F90_OPTS) $< -o $@

vector_analysis.o: autodiff.o vector_analysis.f90
	$(F90) $(F90_OPTS) vector_analysis.f90 -o $@

solver.o: vector_analysis.o solver.f90
	$(F90) $(F90_OPTS) solver.f90 -o $@

gnu_plot.o: gnu_plot.f90
	$(F90) $(F90_OPTS) $< -o $@

libcode.a: $(OBJ)
	ar r $@ $(OBJ)

libcode.so: $(OBJ)
	$(F90) -o $@ $(LINK_OPTS) -shared -Wl,-soname,libcode.so $(OBJ)

archive: $(ARCH_NAME)

$(ARCH_NAME): libcode.so libcode.a
	tar czvf $@ libcode.so libcode.a *.mod

clean:
	@rm -vf libcode.a libcode.so *.mod *.o *~ built.tgz
	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
