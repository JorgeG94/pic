
PIC is named after the Huastec word PIC which means otter.

A work in progress on writing a cool backend for Fortan applications focused on Quantum Chemistry software.

## Building and dependencies

There's two build systems included in the present version, CMake and the [Fortran Package Manager](https://fpm.fortran-lang.org/index.html).

The dependencies of the project are, as of now, CMake (if using cmake), MPI, OpenMP, and a BLAS/LAPACK library.

### CMake

```
mkdir build
cd build
cmake ../
make -j
```

### FPM

Install the FPM following the [instructions](https://fpm.fortran-lang.org/install/index.html#install) and then simply: `fpm build`
