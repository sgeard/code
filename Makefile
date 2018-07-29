.PHONY: all clean help archive test all_archives clean_all_archives

ifdef release
    OBJ_DIR_SUFF := _release
else
    OBJ_DIR_SUFF := _debug
endif

ifdef intel
    ODIR := obj_intel$(OBJ_DIR_SUFF)
    F90 := ifort
    F90_OPTS := -fPIC -fpp -DUSE_AUTODIFF -module $(ODIR)
    ifdef release
        F90_OPTS_EXTRA := #-fp-model precise -fprotect-parens -xHost -prec-sqrt -qopenmp-simd -qopenmp -stand f08
        F90_OPTS += -O3 $(F90_OPTS_EXTRA) -warn all
        ARCH_NAME := build-intel.tgz
    else
        F90_OPTS += -D_DEBUG -g -check bounds -warn all -debug-parameters used -traceback
        ARCH_NAME := build-intel-debug.tgz
    endif
    LINK_OPTS := -static-intel
else
    ODIR := obj_gfortran$(OBJ_DIR_SUFF)
    F90 := /usr/local/bin/gfortran
    F90_OPTS := -c -fPIC -cpp -std=f2008 -fimplicit-none -DUSE_AUTODIFF -ffree-line-length-200 -Wall -Wextra -J$(ODIR)
    ifdef release
        F90_OPTS += -O3
        ARCH_NAME := build-gfortran.tgz
    else
        F90_OPTS += -D_DEBUG -W -ggdb -fbounds-check -ffpe-trap=denormal,invalid
        ARCH_NAME := build-gfortran-debug.tgz
    endif
    LINK_OPTS :=
endif


all: $(ODIR) $(ODIR)/libcode.a $(ODIR)/libcode.so archive

all_archives:
	$(MAKE)
	$(MAKE) release=t
	$(MAKE) intel=t
	$(MAKE) intel=t release=t

SRC := $(wildcard *.f90)
OBJ := ${SRC:%.f90=$(ODIR)/%.o}

$(ODIR):
	mkdir -p $(ODIR)
    
$(ODIR)/autodiff.o: autodiff.f90
	$(F90) $(F90_OPTS) -c $< -o $@
    
$(ODIR)/clib.o: clib.f90
	$(F90) $(F90_OPTS) -c $< -o $@
    
$(ODIR)/bucket.o: bucket.f90
	$(F90) $(F90_OPTS) -c $< -o $@
   
$(ODIR)/bucket_sm.o: bucket_sm.f90 $(ODIR)/bucket.o
	$(F90) $(F90_OPTS) -c $< -o $@
   
$(ODIR)/stl.o: stl.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/stats.o: stats.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/stats_sm.o: stats_sm.f90 $(ODIR)/stats.o
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/vector_analysis.o: $(ODIR)/autodiff.o vector_analysis.f90
	$(F90) $(F90_OPTS) -c vector_analysis.f90 -o $@
    
$(ODIR)/vector_analysis_sm.o: $(ODIR)/autodiff.o vector_analysis_sm.f90
	$(F90) $(F90_OPTS) -c vector_analysis_sm.f90 -o $@

$(ODIR)/solver.o: $(ODIR)/vector_analysis.o solver.f90
	$(F90) $(F90_OPTS) -c solver.f90 -o $@

$(ODIR)/solver_sm.o: $(ODIR)/vector_analysis.o solver_sm.f90
	$(F90) $(F90_OPTS) -c solver_sm.f90 -o $@

$(ODIR)/gnu_plot.o: gnu_plot.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/gnu_plot_sm.o: gnu_plot_sm.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/libcode.a: $(OBJ)
	ar r $@ $(OBJ)

$(ODIR)/libcode.so: $(OBJ)
	$(F90) -o $@ $(OBJ) $(LINK_OPTS) -shared

archive: $(ARCH_NAME)

test_stl: stl.f90 $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) -DTEST_STL $< $(ODIR)/libcode.a

test_stats: stats.f90 archive $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) -DTEST_STATS $< $(ODIR)/libcode.a

$(ARCH_NAME): $(ODIR)/libcode.so $(ODIR)/libcode.a
	tar czvf $@ $(ODIR)/libcode.so $(ODIR)/libcode.a $(ODIR)/*.mod

clean:
	@rm -vrf $(ODIR) *~ $(ARCH_NAME) test_stats
    
clean_all_archives:
	$(MAKE) clean
	$(MAKE) release=t clean
	$(MAKE) intel=t clean
	$(MAKE) intel=t release=t clean

	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
