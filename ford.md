---
src_dir: src
output_dir: docs/fpm-ford
project: PIC
summary: A set of commonly used modules for HPC
project_github: https://github.com/JorgeG94/pic
project_download: https://github.com/JorgeG94/pic/archive/refs/heads/main.zip
author: Jorge Luis Galvez Vallejo
github: https://github.com/JorgeG94
page_dir: docs/pages
media_dir: images
graph: true
graph_maxnodes: 250
graph_maxdepth: 5
coloured_edges: true
display: public
         private
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: images/otter.png
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

PIC is named after the Huastec word PIC which means otter.

<p align="center">
  <img src="./media/pic_logo.png" alt="Otter coding logo" title="Project logo" width="250">
</p>

A work in progress on writing a cool backend for Fortan applications focused on Quantum Chemistry software.

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


## Contributing

Please see the [contributing guidelines](https://jorgeg94.github.io/pic/page/contributing.html) for information on how to contribute to the project.
---
