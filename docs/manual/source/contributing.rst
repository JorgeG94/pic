Contributing
============

Thank you for considering contributing to PIC!

Development Setup
-----------------

1. Fork and clone the repository
2. Install pre-commit hooks:

   .. code-block:: bash

      python3 -m pip install pre-commit
      pre-commit install

3. Build with tests enabled:

   .. code-block:: bash

      cmake -B build -G Ninja -DPIC_ENABLE_TESTING=ON
      cmake --build build

Coding Conventions
------------------

Types and Kinds
^^^^^^^^^^^^^^^

Always use ``integer(default_int)`` from ``pic_types`` - never bare ``integer``, ``integer(4)``, or ``integer(8)``. The project must compile with both ``int32`` (default) and ``int64`` (``-DPIC_DEFAULT_INT8=ON``).

For reals, use ``sp``, ``dp``, ``qp`` from ``pic_types``.

Module Structure
^^^^^^^^^^^^^^^^

One module per file. File name matches module name. Every module follows:

.. code-block:: fortran

   ! SPDX-License-Identifier: MIT
   ! Copyright (c) 2025 Jorge Luis Galvez Vallejo
   !! FORD documentation for the module
   module pic_feature
      !! module doc
      use pic_types, only: default_int, dp
      implicit none
      private
      public :: exported_things
   contains
      ! implementations
   end module pic_feature

- Prefix modules with ``pic_``.
- ``implicit none`` is mandatory.
- ``end module``, ``end subroutine``, ``end function`` must repeat the name.
- ``.f90`` for standard source, ``.F90`` when preprocessor directives are needed.

Documentation
^^^^^^^^^^^^^

Use ``!!`` (double-bang) for FORD documentation comments. Regular ``!`` comments are not included in generated docs.

Formatting and Linting
^^^^^^^^^^^^^^^^^^^^^^

- **fprettify** enforces formatting (runs via pre-commit hook)
- **fortitude** lints the code

Run manually:

.. code-block:: bash

   pre-commit run --all-files

Testing
-------

Tests use the `test-drive <https://github.com/JorgeG94/test-drive>`_ framework.

Adding a New Test
^^^^^^^^^^^^^^^^^

1. Create ``test/test_pic_feature.f90`` with a ``collect_pic_feature_tests`` subroutine
2. Add ``"pic_feature"`` to the ``tests`` list in ``test/CMakeLists.txt``
3. In ``test/main_tests.f90``: add the ``use`` statement, bump the ``allocate(testsuites(N))`` count, and append the ``new_testsuite(...)`` entry

Test pattern:

.. code-block:: fortran

   module test_pic_feature
      use testdrive, only: new_unittest, unittest_type, error_type, check
      implicit none
      private
      public :: collect_pic_feature_tests
   contains
      subroutine collect_pic_feature_tests(testsuite)
         type(unittest_type), allocatable, intent(out) :: testsuite(:)
         testsuite = [new_unittest("test_name", test_procedure)]
      end subroutine

      subroutine test_procedure(error)
         type(error_type), allocatable, intent(out) :: error
         call check(error, condition, "message")
         if (allocated(error)) return
      end subroutine
   end module

Run a single suite:

.. code-block:: bash

   ./build/pic-tester pic_feature

Run a single test:

.. code-block:: bash

   ./build/pic-tester pic_feature test_name

Git Workflow
------------

- Branch naming: ``feat/``, ``fix/``, ``docs/``, ``chore/``, ``experiment/`` prefixes
- PRs target ``main``
- CI must pass before merge (multi-compiler matrix)

Compiler Portability
--------------------

This is the primary concern. Guard compiler-specific code with preprocessor directives:

.. code-block:: fortran

   #ifdef __NVCOMPILER_LLVM__
      ! NVIDIA-specific path
   #endif
   #ifdef _OPENMP
      use omp_lib
   #endif

Test with `Compiler Explorer <https://godbolt.org/>`_ if you don't have access to all compilers. CI tests GNU 10-14, Intel 2024/2025, NVIDIA HPC 25.1, and LFortran.
