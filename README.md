## code

### bucket.f90
The idea is that the calling code fills up the bucket by adding data (in the form of a real(8) array (of rank 1) and
this bucket automatically flushes itself to a file when it's full, so the calling program doesn't need to concern itself
with dynamic memory management. The bucket can be virtual in which case it is never automatically flushed.

### solver.f90  
A collection of Runge-Kutta numerical integration methods:
* rk_exp38      - fourth-order method using the 3/8 rule
* rk_exp4       - fourth-order method using the original method, btter correction than the 3/8 method.
* rk_cash_karp  - fifth order method with error correction
* rk_dop853     - eigther-order method from Dormand & Price
* yfr           - Yoshida-Forest-Ruth 4th-order symplectic integrator

All RK routines were converted (by me) into modern Fortran syntax. yfr was implemented by Claude.

### gnu_plot.f90
Helper module for Fortran code wanting to create gnuplot plots: line, bar, scatter, histograms.
    
### stats.f90
Some basic statistics: mean, standard-deviation and skewness. 3 different ways of creating a histogram.

### vector_analysis.f90
grad, div, curl, div grad using autodiff

### autodiff.f90
Implementation of automatic differentiation

### dlist.f90
Doubly linked-list

## Makefile
For building everything.

### Archives
- *Intel* archives are built using ifx version 2025.2.1
- *Gfortran* archves are built using GNU Fortran (GCC) 8.1.0

Each archive contains the following:

* libcode.so
* libcode.a
* auto_diff.mod
* bucket.mod
* clib.mod
* gnu_plot.mod
* solver.mod
* stats.mod
* stl.mod
* vector_analysis.mod

Supported archives are as follows:
* build-gfortran-debug.tgz
* build-gfortran-release.tgz
* build-intel-debug.tgz
* build-intel-release.tgz

