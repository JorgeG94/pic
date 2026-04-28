Compiler Portability
====================

Compiler portability is PIC's primary concern. The library is tested and maintained to work across multiple Fortran compilers.

Supported Compilers
-------------------

PIC officially supports:

- **GNU Fortran (gfortran)** - Versions 10-14
- **Intel Fortran (ifx/ifort)** - 2024, 2025
- **NVIDIA HPC SDK (nvfortran)** - 25.1+
- **LFortran** - Latest
- **Cray Fortran (crayftn)** - Latest
- **Classic Flang** - Latest

Why Portability Matters
-----------------------

The Fortran ecosystem has multiple compilers, each with their own quirks and levels of standard compliance. Many Fortran libraries (including parts of stdlib) fail to compile on certain compilers.

PIC exists specifically to provide utilities that work everywhere. If code compiles with GNU but not NVIDIA, it's a bug in PIC that needs to be fixed.

Writing Portable Code
---------------------

Preprocessor Guards
^^^^^^^^^^^^^^^^^^^

Use preprocessor directives for compiler-specific code:

.. code-block:: fortran

   #ifdef __GFORTRAN__
      ! GNU-specific code
   #endif

   #ifdef __INTEL_COMPILER
      ! Intel-specific code
   #endif

   #ifdef __NVCOMPILER_LLVM__
      ! NVIDIA-specific code
   #endif

   #ifdef _OPENMP
      use omp_lib
   #endif

Common Portability Issues
^^^^^^^^^^^^^^^^^^^^^^^^^

1. **Assumed-rank arrays** - Not all compilers support ``dimension(..)``. Use explicit ranks when possible.

2. **Coarrays** - Support varies widely. Guard with ``#ifdef`` checks.

3. **IEEE modules** - ``ieee_arithmetic`` support differs. Test carefully.

4. **Intrinsic function behavior** - Some intrinsics behave slightly differently across compilers.

5. **OpenMP support** - Always guard OpenMP code with ``#ifdef _OPENMP``.

Testing Portability
^^^^^^^^^^^^^^^^^^^

If you don't have access to all compilers locally:

1. Use `Compiler Explorer (Godbolt) <https://godbolt.org/>`_ to test compilation
2. Submit a PR and let CI test across the full compiler matrix
3. Check CI logs carefully for compiler-specific warnings

File Extensions
^^^^^^^^^^^^^^^

- Use ``.f90`` for standard Fortran source
- Use ``.F90`` when preprocessor directives are needed (the file goes through cpp)

CI Testing Matrix
-----------------

Every PR is tested against:

- GNU Fortran 10, 11, 12, 13, 14
- Intel oneAPI 2024, 2025
- NVIDIA HPC SDK 25.1
- LFortran (latest)

All compilers must pass before merge.

Reporting Compiler Issues
-------------------------

If you find code that doesn't compile on a specific compiler:

1. Open an issue with the compiler name and version
2. Include the exact error message
3. Provide a minimal reproducing example if possible

We take portability issues seriously and will work to fix them promptly.
