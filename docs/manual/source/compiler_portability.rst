Compiler Portability
====================

Compiler portability is PIC's primary concern. The library is tested and maintained to work across multiple Fortran compilers.

Supported Compilers
-------------------

PIC officially supports:

- **GNU Fortran (gfortran)** - Versions 10-15
- **Intel Fortran (ifx)** - 2024.1, 2025.0
- **Intel Fortran Classic (ifort)** - 2021.10
- **NVIDIA HPC SDK (nvfortran)** - latest
- **AOCC (classic flang)** - 5.1.0
- **LLVM Flang (flang-new)** - latest
- **LFortran** - latest
- **Cray Fortran (crayftn)** - latest

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

.. warning::

   ``__FLANG`` and ``__flang__`` are **two different compilers**. Classic
   flang (as shipped in AOCC) defines ``__FLANG``; LLVM flang (``flang-new``)
   defines ``__flang__``. A guard written as ``#if !defined(__FLANG)`` does
   **not** exclude LLVM flang.

   That is usually what you want — LLVM flang supports far more of the modern
   standard than classic flang does, including user-defined derived-type I/O,
   which classic flang and nvfortran do not. Before adding ``__flang__`` to an
   exclusion, check whether LLVM flang actually needs excluding; assuming it
   behaves like its namesake will silently disable working functionality.

Common Portability Issues
^^^^^^^^^^^^^^^^^^^^^^^^^

These are drawn from bugs actually found in PIC, not from a general style
guide. Each one compiled cleanly on at least one compiler before it was
caught.

1. **Signed integer overflow is undefined.** Fortran has no unsigned type, and
   a wrapping multiply is not something the standard grants you —
   ``-ftrapv``, ``-fsanitize=signed-integer-overflow`` and Intel's ``-check``
   all abort on it. Do modular arithmetic in a wider kind and reduce
   explicitly, or split operands into limbs. PIC's FNV hash and ``pic_rng``
   both do this.

2. **BOZ literals in initialization expressions.** gfortran, ifx and
   nvfortran each accept and warn about these differently, and a constant with
   the top bit set is not a representable positive integer at all. Write the
   signed decimal equal to the intended two's-complement pattern, with the hex
   in a comment.

3. **``transfer`` between real and integer kinds.** nvfortran and LFortran
   disagree with GNU and Intel on some inputs. Use ``fraction`` and
   ``exponent`` when you need to decompose a real portably.

4. **``merge`` evaluates both value arguments.** It is an ordinary function
   reference, not a conditional expression, so
   ``merge(f(x%raw), default, allocated(x%raw))`` references an unallocated
   allocatable whenever the mask is false. Use ``if``/``else``.

5. **Zero-sized arrays.** Allocating ``buf(0:-1)`` is legal, but walking into
   code that indexes it is not. LFortran's runtime bounds checking is the only
   thing in the matrix that reliably catches this; on other compilers it reads
   whatever is adjacent, which can hang or crash depending on what it finds.
   Return early below two elements.

6. **Unformatted sequential record markers** are processor dependent in width
   and endianness. Use stream access with your own envelope if a file must be
   read by a different compiler than wrote it.

7. **List-directed and ``g0`` output are processor dependent** in digit count,
   trailing zeros, leading zero, exponent letter and exponent width. Use
   ``pic_format`` when output must be compared byte-for-byte.

8. **``system_clock`` may report no clock at all**, in which case the count
   rate is zero. Guard before dividing by it.

9. **Assumed-rank arrays** - Not all compilers support ``dimension(..)``. Use
   explicit ranks when possible.

10. **Coarrays** - Support varies widely. Guard with ``#ifdef`` checks.

11. **IEEE modules** - ``ieee_arithmetic`` support differs. Test carefully.

12. **OpenMP support** - Always guard OpenMP code with ``#ifdef _OPENMP``.

13. **Procedure-pointer components** in derived types are a recurring problem
    across this matrix. Prefer an explicit enum or a separate array argument
    over a comparator or callback stored in a type.

14. **``-ffast-math`` folds ``x == x`` to true**, so a self-comparison is not a
    portable NaN test. Compare against a bound instead: NaN compares false
    against every bound, so ``x < limit`` rejects it.

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

**Linux, CMake** (``multi-compiler-ci.yml``)
   GNU 10, 11, 12, 13, 14 and 15; Intel Classic 2021.10; Intel 2024.1 and
   2025.0; NVIDIA HPC (latest); AOCC 5.1.0.

**Linux, macOS and Windows, FPM and CMake** (``conda-multi-compiler-ci.yml``)
   gfortran, ifx and LLVM flang-new, minus the combinations that do not work
   upstream (ifx has no macOS build; flang-new has no conda-forge package on
   macOS and is not ready on Windows).

**LFortran** (``lfortran-ci.yml``)
   Linux, macOS and Windows.

**Generated sources** (``check-autogen.yml``)
   Every fypp-generated file is regenerated and compared against its committed
   output. See :doc:`contributing`.

All compilers must pass before merge. The workflow files are the authority
here — if this list and the YAML disagree, the YAML is right and this page
needs updating.

Known Compiler Bugs
-------------------

Where PIC works around a compiler defect rather than a standard requirement,
the workaround is written so that it **stays correct once the defect is
fixed** — preferring a standard-equivalent spelling over a ``#ifdef``, so
there is nothing to remember to delete.

Current LFortran 0.65.0 defects, all filed upstream with standalone
reproducers:

1. **A ``DT`` edit descriptor's v-list arrives in ``iotype``.** For ``DT(5)``,
   F2018 12.6.4.8.3 requires ``iotype == 'DT'`` and ``v_list == [5]``.
   LFortran passes ``iotype == 'DT(5)'`` with a zero-sized ``v_list``, so the
   values are unreachable. This is the one place PIC needs a compiler guard.

2. **``LEN()`` of a zero-sized assumed-length character array** is evaluated by
   indexing element 0. ``LEN`` is a type-parameter inquiry and must not
   require an element to exist.

3. **ICE on ``IS_CONTIGUOUS`` through a generic interface.**
   ``get_struct_sym_from_struct_expr() not implemented for 115``. The same
   expression is accepted in an assignment, in an output list, under ``.not.``
   and as an argument of a directly-named specific procedure — only the
   generic name fails. Worked around by assigning to a local ``logical``
   first, which is standard-equivalent and needs no guard.

4. **``OPEN(STATUS='REPLACE')`` into a nonexistent directory returns
   ``IOSTAT=0``** and creates no file; the following ``WRITE`` and ``CLOSE``
   also succeed, so data is silently discarded. Code that checks ``iostat`` to
   confirm a checkpoint was written will believe it succeeded. PIC's test for
   this probes the processor first and asserts only where the failure is
   reportable, so it starts testing again by itself once this is fixed.

Other compilers:

- **nvfortran** does not set ``stat`` reliably for deferred-length character
  array allocations. Test ``allocated(...)`` instead, which always reports the
  real post-allocation state.
- **ifx** corrupts its own heap in namelist machinery involving
  ``string_type``, on both Linux and Windows, while handling ``dt`` edit
  descriptors correctly.
- **nvfortran and classic flang** do not support user-defined derived-type
  I/O; ``string_type``'s defined-I/O interfaces are excluded on both.

Reporting Compiler Issues
-------------------------

If you find code that doesn't compile on a specific compiler:

1. Open an issue with the compiler name and version
2. Include the exact error message
3. Provide a minimal reproducing example if possible

We take portability issues seriously and will work to fix them promptly.
