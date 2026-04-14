Installation
============

PIC supports two build systems: **CMake** (primary) and **FPM** (Fortran Package Manager).

Requirements
------------

- A Fortran compiler:
   - GNU Fortran (gfortran) 10+
   - Intel Fortran (ifx/ifort) 2024+
   - NVIDIA HPC SDK (nvfortran) 25.1+
   - LFortran (latest)
   - Cray Fortran (crayftn)
   - Classic Flang

- Build tools:
   - CMake 3.31+ (for CMake builds)
   - FPM 0.12.0+ (for FPM builds)

- An internet connection (to fetch the ``test-drive`` dependency)

Building with CMake
-------------------

Basic build:

.. code-block:: bash

   cmake -B build -G Ninja
   cmake --build build

Build with testing enabled:

.. code-block:: bash

   cmake -B build -G Ninja -DPIC_ENABLE_TESTING=ON
   cmake --build build
   ctest --test-dir build -V

Install to a custom location:

.. code-block:: bash

   export PIC_ROOT=$HOME/install/pic
   cmake -B build -G Ninja -DCMAKE_INSTALL_PREFIX=$PIC_ROOT
   cmake --build build --target install

CMake Options
^^^^^^^^^^^^^

+------------------------+---------+-------------------------------------------+
| Option                 | Default | Description                               |
+========================+=========+===========================================+
| ``PIC_DEFAULT_INT8``   | OFF     | Use 64-bit default integers               |
+------------------------+---------+-------------------------------------------+
| ``PIC_ENABLE_OMP``     | OFF     | Enable OpenMP parallelization             |
+------------------------+---------+-------------------------------------------+
| ``PIC_ENABLE_BLAS``    | OFF     | Enable BLAS-backed operations             |
+------------------------+---------+-------------------------------------------+
| ``PIC_ENABLE_MPI``     | OFF     | Enable MPI support                        |
+------------------------+---------+-------------------------------------------+
| ``PIC_ENABLE_TESTING`` | ON      | Build the test suite                      |
+------------------------+---------+-------------------------------------------+

Building with FPM
-----------------

Basic build:

.. code-block:: bash

   fpm build --profile release

Build and run tests:

.. code-block:: bash

   fpm test --profile release

Install:

.. code-block:: bash

   fpm install --prefix $HOME/install/pic --profile release

Using PIC in Your Project
-------------------------

With CMake
^^^^^^^^^^

After installing PIC, use ``find_package`` in your CMakeLists.txt:

.. code-block:: cmake

   cmake_minimum_required(VERSION 3.14)
   project(my_app LANGUAGES Fortran)

   # Set pic_ROOT to your PIC installation path
   find_package(pic REQUIRED)

   add_executable(my_app main.f90)
   target_link_libraries(my_app PRIVATE pic::pic)

.. note::

   CMake uses ``pic_ROOT`` or ``pic_DIR`` to locate PIC. Set the environment variable:

   .. code-block:: bash

      export pic_ROOT=/path/to/pic/install

With FPM
^^^^^^^^

Add PIC as a dependency in your ``fpm.toml``:

.. code-block:: toml

   [dependencies]
   pic = { git = "https://github.com/JorgeG94/pic.git", branch = "main" }

Then use it in your code:

.. code-block:: fortran

   program my_app
      use pic
      implicit none

      call log_info("PIC is working!")
   end program my_app

Platform Notes
--------------

Linux
^^^^^

PIC builds out of the box on most Linux distributions. Tested on x86_64 and ARM64.

macOS
^^^^^

Use Homebrew to install compilers:

.. code-block:: bash

   brew install gcc

.. warning::

   Using conda-installed compilers on macOS may introduce issues. Homebrew is recommended.

Windows
^^^^^^^

PIC has been successfully built using Windows Subsystem for Linux (WSL).
