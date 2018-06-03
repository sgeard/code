.PHONY: all clean help archive test

ifdef intel
    ODIR := obj_intel
    F90 := ifort
    F90_OPTS := -fPIC -fpp -DUSE_AUTODIFF -module $(ODIR)
    ifdef release
        F90_OPTS += -O3
    else
        F90_OPTS += -D_DEBUG -g -check bounds
    endif
    LINK_OPTS := -static-intel
    ARCH_NAME := build-intel.tgz
else
    ODIR := obj_gfortran
    F90 := /usr/local/bin/gfortran
    F90_OPTS := -c -fPIC -cpp -std=f2008 -fimplicit-none -DUSE_AUTODIFF -ffree-line-length-200 -Wall -Wextra -J$(ODIR)
    ifdef release
        F90_OPTS += -O3
    else
        F90_OPTS += -D_DEBUG -W -ggdb -fbounds-check -ffpe-trap=denormal,invalid
    endif
    LINK_OPTS :=
    ARCH_NAME := build.tgz
endif


all: $(ODIR) $(ODIR)/libcode.a $(ODIR)/libcode.so archive

SRC := $(wildcard *.f90)
OBJ := ${SRC:%.f90=$(ODIR)/%.o}

$(ODIR):
	mkdir -p $(ODIR)
    
$(ODIR)/autodiff.o: autodiff.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/stats.o: stats.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/stats_sm.o: stats_sm.f90
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

test_stats: stats.f90 archive
	$(F90) -o $@ $(F90_OPTS) -DTEST_STATS $< libcode.a
   
$(ARCH_NAME): $(ODIR)/libcode.so $(ODIR)/libcode.a
	tar czvf $@ $(ODIR)/libcode.so $(ODIR)/libcode.a $(ODIR)/*.mod

clean:
	@rm -vrf $(ODIR) *~ built.tgz test_stats
	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
