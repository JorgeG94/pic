[![GNU cross OS build](https://github.com/JorgeG94/pic/actions/workflows/cmake-build.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/cmake-build.yml)
[![Deploy FORD Docs](https://github.com/JorgeG94/pic/actions/workflows/deploy-docs.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/deploy-docs.yml)
[![GitHub top language](https://img.shields.io/github/languages/top/JorgeG94/pic)](https://github.com/JorgeG94/pic)
![License](https://img.shields.io/github/license/JorgeG94/pic)
![Last Commit](https://img.shields.io/github/last-commit/JorgeG94/pic)

PIC is named after the Huastec word PIC which means otter.

A work in progress on writing a cool backend for Fortan applications focused on Quantum Chemistry software.

## Building and dependencies

There's two build systems included in the present version, CMake and the [Fortran Package Manager](https://fpm.fortran-lang.org/index.html).

The dependencies of the project are, as of now, CMake (if using cmake), MPI, OpenMP, and a BLAS/LAPACK library.

You can find the the source docs [here](https://jorgeg94.github.io/pic/)

### CMake

CMake offers a very modular build of PIC, the following options are available and can be triggered by `-DPIC_ENABLE_XYZ=ON/OFF`

| Option Name            | Default | Description                                |
|------------------------|---------|--------------------------------------------|
| `PIC_DEFAULT_INT8`     | `OFF`   | Use 8-byte integers as default             |
| `PIC_ENABLE_MPI`       | `OFF`   | Enable the use of MPI in PIC               |
| `PIC_ENABLE_OMP`       | `OFF`   | Enable the use of OpenMP in PIC            |
| `PIC_ENABLE_BLAS`      | `OFF`   | Enable BLAS/LAPACK libraries in PIC        |
| `PIC_ENABLE_TESTING`   | `ON`    | Enable testing for PIC                     |
| `PIC_ENABLE_JONQUIL`   | `OFF`   | Enable Jonquil for JSON/TOML handling      |


```
mkdir build
cd build
cmake ../
make -j
ctest
```

### FPM

Install the FPM following the [instructions](https://fpm.fortran-lang.org/install/index.html#install) and then simply: `fpm build`
