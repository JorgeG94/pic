Installation
============

PIC supports two build systems: **CMake** (primary) and **FPM**.

Requirements
------------

- A Fortran compiler (GNU, Intel, NVIDIA HPC, LFortran, Cray, or Flang)
- CMake 3.14+ (for CMake builds)
- FPM (for FPM builds)

Building with CMake
-------------------

.. code-block:: bash

   cmake -B build -G Ninja -DPIC_ENABLE_TESTING=ON
   cmake --build build
   ctest --test-dir build -V

CMake Options
^^^^^^^^^^^^^

- ``PIC_ENABLE_OMP`` - Enable OpenMP support
- ``PIC_DEFAULT_INT8`` - Use 64-bit default integers
- ``PIC_ENABLE_BLAS`` - Enable BLAS support
- ``PIC_ENABLE_JONQUIL`` - Enable Jonquil JSON support
- ``PIC_ENABLE_MPI`` - Enable MPI support
- ``PIC_ENABLE_TESTING`` - Build tests

Building with FPM
-----------------

.. code-block:: bash

   fpm build --profile release
   fpm test --profile release

Using PIC in Your Project
-------------------------

CMake
^^^^^

After installing PIC, you can use it in your CMake project:

.. code-block:: cmake

   find_package(pic REQUIRED)
   target_link_libraries(your_target PRIVATE pic::pic)

FPM
^^^

Add PIC as a dependency in your ``fpm.toml``:

.. code-block:: toml

   [dependencies]
   pic = { git = "https://github.com/JorgeG94/pic.git" }
