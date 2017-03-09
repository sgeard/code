.PHONY: all clean help archive

all: libcode.a libcode.so

SRC := $(wildcard  *.f90)
OBJ := ${SRC:%.f90=%.o}

F90 := gfortran
F90_OPTS := -c -fPIC -cpp -O3

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
	gfortran -o $@ -shared -Wl,-soname,libcode.so $(OBJ)

archive: built.tgz

built.tgz: libcode.so libcode.a
	tar czvf built.tgz libcode.so libcode.a *.mod

clean:
	@rm -vf libcode.a libcode.so *.mod *.o *~ built.tgz
	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
