.PHONY: all clean help archive test all_archives clean_all_archives debug_avd run_avd

ifdef release
    OBJ_DIR_SUFF := _release
else
    OBJ_DIR_SUFF := _debug
endif

ifdef intel
    ODIR := obj_intel$(OBJ_DIR_SUFF)
    F90 := ifx
    F90_OPTS := -fPIC -fpp -module $(ODIR)
    ifdef release
        F90_OPTS_EXTRA := -xHost -fp-model precise -qopenmp -qopenmp-simd
        F90_OPTS += -O3 $(F90_OPTS_EXTRA) -warn all
        ARCH_NAME := build-intel-release.tgz
    else
        F90_OPTS += -D_DEBUG -g -check bounds -warn all -traceback -debug-parameters used
        ARCH_NAME := build-intel-debug.tgz
    endif
    LINK_OPTS := -static-intel
else
    ODIR := obj_gfortran$(OBJ_DIR_SUFF)
    F90 := gfortran
    F90_OPTS := -fPIC -cpp -std=f2018 -fimplicit-none -ffree-line-length-200 -Wall -Wextra -J$(ODIR)
    ifdef release
        F90_OPTS += -O3
        ARCH_NAME := build-gfortran-release.tgz
    else
        F90_OPTS += -D_DEBUG -Wall -ggdb -fbounds-check -ffpe-trap=denormal,invalid
        ARCH_NAME := build-gfortran-debug.tgz
    endif
    LINK_OPTS :=
endif


all: $(ODIR) $(ODIR)/libcode.a $(ODIR)/libcode.so archive

planets_lib: $(ODIR) $(PLANETS_OBJ)
	ar r $(ODIR)/libcode.a $(PLANETS_OBJ)

all_archives:
	$(MAKE)
	$(MAKE) release=t
	$(MAKE) intel=t
	$(MAKE) intel=t release=t

SRC := avd.f90 avd_sm.f90 clib.f90 bucket_sm.f90 gnu_plot.f90 solver.f90 solver_sm.f90 stats_sm.f90 vector_analysis.f90 bucket.f90 gnu_plot_sm.f90 stats.f90 vector_analysis_sm.f90 trace.f90 dlist.f90 dlist_sm.f90
OBJ := ${SRC:%.f90=$(ODIR)/%.o}

PLANETS_SRC := avd.f90 avd_sm.f90 clib.f90 vector_analysis.f90 vector_analysis_sm.f90 solver.f90 solver_sm.f90 gnu_plot.f90 gnu_plot_sm.f90 stats.f90 stats_sm.f90 bucket.f90 bucket_sm.f90
PLANETS_OBJ := ${PLANETS_SRC:%.f90=$(ODIR)/%.o}

EXTRA_EXES := binomial rbtree avu AG

debug_avd: avd
	gdb avd -x gdb_comm

run_avd: avd
	avd

binomial: binomial.f90
	$(F90) $< -o $@

rbtree: rbtree.f90 $(ODIR)
	$(F90) $(F90_OPTS) $< -o $@

avd: $(ODIR) avd.f90 avd_sm.f90 avd_functions.f90 av_utest.f90 Makefile
	$(F90) $(F90_OPTS) avd.f90 avd_sm.f90 avd_functions.f90 av_utest.f90 -o $@

avu: avu.f90 avd.f90 avd_times_sm.f90  avd_functions.f90 av_utest.f90 Makefile avd_times_sm.f90
	$(F90) $(F90_OPTS) avu.f90 avd.f90 avd_sm.f90 avd_functions.f90 av_utest.f90 -o $@

AG: AG.f90 Makefile
	$(F90) $(F90_OPTS) AG.f90 -DUTEST -o $@

$(ODIR):
	mkdir -p $(ODIR)

$(ODIR)/libcode.a: $(OBJ)
	ar r $@ $(OBJ)

$(ODIR)/libcode.so: $(OBJ)
	$(F90) -o $@ $(OBJ) $(LINK_OPTS) -shared

$(ODIR)/avd_times_sm.o: $(ODIR)/avd.o avd_times_sm.f90
	$(F90) $(F90_OPTS) -c avd_times_sm.f90 -o $@

$(ODIR)/avd.o: ../autodiff/avd.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/avd_sm.o: ../autodiff/avd_sm.f90 $(ODIR)/avd.o
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

$(ODIR)/vector_analysis.o: $(ODIR)/avd.o vector_analysis.f90
	$(F90) $(F90_OPTS) -c vector_analysis.f90 -o $@

$(ODIR)/vector_analysis_sm.o: $(ODIR)/avd.o vector_analysis_sm.f90
	$(F90) $(F90_OPTS) -c vector_analysis_sm.f90 -o $@

$(ODIR)/solver.o: $(ODIR)/vector_analysis.o solver.f90
	$(F90) $(F90_OPTS) -c solver.f90 -o $@

$(ODIR)/solver_sm.o: $(ODIR)/vector_analysis.o solver_sm.f90
	$(F90) $(F90_OPTS) -c solver_sm.f90 -o $@

$(ODIR)/trace.o: trace.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/gnu_plot.o: gnu_plot.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/gnu_plot_sm.o: gnu_plot_sm.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/dlist.o: dlist.f90
	$(F90) $(F90_OPTS) -c $< -o $@

$(ODIR)/dlist_sm.o: dlist_sm.f90 $(ODIR)/dlist.mod
	$(F90) $(F90_OPTS) -c $< -o $@

archive: $(ARCH_NAME)

test_stl: $(ODIR) stl.f90 $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) -DTEST_STL $< $(ODIR)/libcode.a

test_stats: $(ODIR) stats.f90 archive $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) -DTEST_STATS $< $(ODIR)/libcode.a

test_bucket: $(ODIR) bucket.f90 archive $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) -DTEST_BUCKET $< $(ODIR)/libcode.a

utest_dlist: $(ODIR) utest_dlist.f90 $(ODIR)/libcode.a
	$(F90) -o $@ $(F90_OPTS) utest_dlist.f90 $(ODIR)/libcode.a

$(ARCH_NAME): $(ODIR)/libcode.so $(ODIR)/libcode.a
	tar czvf $@ $(ODIR)/libcode.so $(ODIR)/libcode.a $(ODIR)/*.mod
	
clean:
	@rm -vrf $(ODIR) *~ $(ARCH_NAME) test_stats $(EXTRA_EXES) *.mod
    
clean_all_archives:
	$(MAKE) clean
	$(MAKE) release=t clean
	$(MAKE) intel=t clean
	$(MAKE) intel=t release=t clean

	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
