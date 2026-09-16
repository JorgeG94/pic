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

Generated Sources
-----------------

Several modules are **generated from fypp templates at development time**, and
both the template and its output are committed. Editing the generated file
directly is the single easiest way to have your change silently reverted by the
next person who regenerates.

Files under ``tools/autogen/`` ending in ``.fypp`` are the source of truth for:

- ``src/lib/core/arrays/pic_array.f90``
- ``src/lib/core/hash/pic_hash_32bit.f90``, ``pic_hash_32bit_fnv.f90``
- ``src/lib/core/strings/pic_ascii.f90``, ``pic_strings.f90``,
  ``pic_string_type.F90``, ``pic_string_type_constructor.f90``,
  ``pic_strings_to_strings.F90``
- ``src/lib/core/soa/pic_soa_particle.f90`` (from ``pic_soa_particle.fypp``,
  which is two lines over the ``pic_soa.fypp`` macro library --- edit that one
  only to change the generator itself, never to add a container of your own;
  see :doc:`features`)

To change any of them:

.. code-block:: bash

   # 1. edit the template under tools/autogen/
   # 2. regenerate (needs fypp and fprettify)
   cd tools/autogen && ./autogen.sh
   # 3. commit BOTH the template and the regenerated source

Check your work before pushing:

.. code-block:: bash

   tools/autogen/check_generated.sh

It regenerates every template into a temporary directory and diffs against the
committed output, so it never writes into ``src/``. CI runs the same script
(``check-autogen.yml``); a mismatch fails the build.

Other CI Checks
---------------

Two checks beyond the compiler matrix, both runnable locally:

.. code-block:: bash

   tools/autogen/check_generated.sh          # templates match their output
   tools/ci/check_no_session_links.sh        # no assistant session URLs

The second scans both commit messages and lines added to files. An assistant
session URL points at a private transcript that resolves for nobody else, and
once it is in a commit message it can only be removed by rewriting history —
so it is refused while it is still cheap to fix. Co-authorship trailers are
not matched and are fine to keep.

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

Test with `Compiler Explorer <https://godbolt.org/>`_ if you don't have access to all compilers. See :doc:`compiler_portability` for the full CI matrix and for the portability pitfalls that have actually bitten this project.
