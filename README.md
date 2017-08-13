## code
### autodiff.f90
implementation of automatic differentiation
### vector_analysis.f90
grad, div, curl, div grad using autodiff
### solver.f90  
ODE solver with FFT calculator. Originally from Numerical Recipes 77 and recast using the modern tools
### gnu_plot.f90
helper module for Fortran code wanting to create gnuplot plots
### stats.f90
Some basic statistics: mean, standard-deviation and skewness. 3 different ways of creating a histogram.
## Makefile
for building
### built.tgz
archive library, shared library and mod files - gfortran
### built-intel.tgz
archive library, shared library and mod files - Intel
