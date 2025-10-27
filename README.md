[![GNU cross OS build](https://github.com/JorgeG94/pic/actions/workflows/local_conda_env.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/local_conda_env.yml)
[![Linux multi compiler setup](https://github.com/JorgeG94/pic/actions/workflows/multi-compiler-ci.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/multi-compiler-ci.yml)
[![LFortran build](https://github.com/JorgeG94/pic/actions/workflows/lfortran-ci.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/lfortran-ci.yml)
[![Deploy FORD Docs](https://github.com/JorgeG94/pic/actions/workflows/deploy-docs.yml/badge.svg)](https://github.com/JorgeG94/pic/actions/workflows/deploy-docs.yml)
[![GitHub top language](https://img.shields.io/github/languages/top/JorgeG94/pic)](https://github.com/JorgeG94/pic)
![License](https://img.shields.io/github/license/JorgeG94/pic)
![Last Commit](https://img.shields.io/github/last-commit/JorgeG94/pic)
[![codecov](https://codecov.io/github/JorgeG94/pic/graph/badge.svg?token=6A6PGOZ7GW)](https://codecov.io/github/JorgeG94/pic)

PIC is named after the Huastec word PIC which means otter.

<p align="center">
  <img src="images/pic_logo.png" alt="Otter coding logo" title="Project logo" width="250">
</p>

A work in progress on writing a cool backend for Fortan applications focused on scientific computing software. Originally this was going
to be for quantum chemistry but I ended up writing way more general routines than I expected.

## Contributing

Please see the [contributing guidelines](https://jorgeg94.github.io/pic/page/contributing.html) for information on how to contribute to the project.

See our [code of conduct](CODE_OF_CONDUCT.md) for details on community standards. In short, PIC is a welcoming codebase that is open to contributions
from anyone at any level of experience. Do you want to fix my thousands of typos, go ahead. Do you want to contribute code, go ahead. Just always
be respectful of others.

## Building and dependencies

There's two build systems included in the present version, CMake and the [Fortran Package Manager](https://fpm.fortran-lang.org/index.html).

The dependencies of the project are, as of now, CMake (if using cmake), MPI, OpenMP, and a BLAS/LAPACK library.

## Documentation

The code itself is documented using [FORD](https://forddocs.readthedocs.io/en/stable/) and the documentation is available [here](https://jorgeg94.github.io/pic/).

Comments in the code that are prefixed with `!!` are considered documentation comments and will be processed by FORD. Comments without that prefix are considered regular comments and will not be processed by FORD. So, please do not use `!!` for your comments unless you want them to be included in the documentation.

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

#### Advanced options:

| Option Name            | Default | Description                                |
|------------------------|---------|--------------------------------------------|
| `PIC_USE_VAPAA`        | `OFF`   | Use vapaa for binding to MPI               |

Information on vapaa see [here](https://github.com/JorgeG94/vapaa/tree/main) which is my
personal fork which is pulled from here, and Jeff's [project](https://github.com/jeffhammond/vapaa).

Will update to use the orignal project at a later date.


### Building:

```
mkdir build
cd build
cmake ../
make -j
ctest
```

### FPM

Install the FPM following the [instructions](https://fpm.fortran-lang.org/install/index.html#install) and then simply: `fpm build`


## Examples

You can see some [WIP] examples in the [pic-examples](https://github.com/JorgeG94/pic_examples) repository.

## Using PIC in your work

### Fortran Package Manager

Simply add:
```

[dependencies]
pic = { git = "https://github.com/JorgeG94/pic.git", branch = "main"}
```

to your fpm.toml file and you'll be able to checkout and use pic.

### CMake

For CMake it is a bit more complex, since you'll need to pull the dependency. You can see this [template repo](https://github.com/JorgeG94/pic-app-sample), which
serves as an example on pulling and using the code inside your build system.

### Static/Shared linking via CMake

pic is compiled with "CMake symbols", i.e. it will be findable by a CMake package provided you do the right things. Using
`find_package(pic REQUIRED)` will use CMake intrinsics to try to find the necessary things to link to pic. pic comes with the
target `pic::pic` that you can use for your `target_link_libraries(${my_target} PRIVATE pic::pic)`. This will import
all includes, modules, libs, etc.

How CMake finds `pic::pic` depends on the policy `CMP0074`, this controls if the variables `pic_DIR` or `pic_ROOT` are used
to search for the package. If you have set the policy: `cmake_policy(SET CMP0074 NEW)`, then `pic_ROOT` will also be used,
otherwise it is *IGNORED*. By setting `export pic_ROOT=/path/to/where/pic/is/installed` it will let CMake find the
necessary files it needs to just link pic. Be careful that, as of now, pic needs to be in small letter. All caps will fail to
find.
