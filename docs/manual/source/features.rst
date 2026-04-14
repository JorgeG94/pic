Features
========

PIC provides a comprehensive set of utilities for Fortran development.

Core Modules
------------

Types (``pic_types``)
^^^^^^^^^^^^^^^^^^^^^

Portable kind definitions that work across all supported compilers:

- ``default_int`` - Default integer kind (32 or 64-bit, compile-time configurable)
- ``sp``, ``dp``, ``qp`` - Single, double, and quad precision real kinds
- ``int8``, ``int16``, ``int32``, ``int64`` - Fixed-width integer kinds

Strings (``pic_strings``, ``pic_string_type``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Comprehensive string handling:

- Dynamic string type (``string_type``)
- Conversion functions (``to_string``, ``to_lower``, ``to_upper``)
- String manipulation utilities
- ASCII character utilities (``pic_ascii``)

Logger (``pic_logger``)
^^^^^^^^^^^^^^^^^^^^^^^

Logging utilities with multiple severity levels:

- ``log_info`` - Informational messages
- ``log_warning`` - Warning messages
- ``log_error`` - Error messages
- ``log_debug`` - Debug messages

A pure logger variant (``pic_pure_logger``) is also available for use in pure procedures.

Timer (``pic_timer``)
^^^^^^^^^^^^^^^^^^^^^

High-resolution timing utilities:

- Start/stop timing
- Elapsed time measurement
- Support for nested timers

Arrays (``pic_array``)
^^^^^^^^^^^^^^^^^^^^^^

Array utilities:

- ``fill_vector`` - Fill arrays with values
- Support for multiple data types and ranks
- Optional OpenMP parallelization

Sorting (``pic_sorting``)
^^^^^^^^^^^^^^^^^^^^^^^^^

Sorting algorithms that work across all compilers:

- Multiple sorting algorithms
- Support for various data types
- Works where stdlib sorting may not compile

Hash Functions (``pic_hash_32bit``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

32-bit hash functions:

- FNV-1a hash implementation
- General-purpose hashing utilities

I/O (``pic_io``)
^^^^^^^^^^^^^^^^

Input/output utilities:

- File handling helpers
- Formatted output utilities

Constants (``pic_constants``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Mathematical and physical constants in various precisions.

Helpers (``pic_helpers``, ``pic_optional``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

General helper functions:

- Optional argument handling
- Common utility functions

Command Line (``pic_command_line``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Command-line argument parsing utilities.

Performance Modules
-------------------

FLOP Recorder (``pic_flop_recorder``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Track floating-point operations in your code.

FLOP Rate (``pic_flop_rate``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Measure FLOP rates for performance analysis.

Knowledge Base (``pic_knowledge``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hardware and system information utilities.

Optional Features
-----------------

These features require additional dependencies or compiler flags:

OpenMP Support
^^^^^^^^^^^^^^

Enable with ``-DPIC_ENABLE_OMP=ON``. Provides parallel implementations of various operations.

BLAS Support
^^^^^^^^^^^^

Enable with ``-DPIC_ENABLE_BLAS=ON``. Provides BLAS-backed linear algebra operations.

MPI Support
^^^^^^^^^^^

Enable with ``-DPIC_ENABLE_MPI=ON``. Provides MPI-aware utilities.
