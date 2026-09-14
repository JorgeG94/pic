Features
========

PIC provides a comprehensive set of utilities for Fortran development.

Every module here compiles on the full CI matrix. Where a module exists
because the standard leaves something *processor dependent*, that reason is
stated — it is usually the most important thing to know about the module.

Core Modules
------------

Types (``pic_types``)
^^^^^^^^^^^^^^^^^^^^^

Portable kind definitions that work across all supported compilers:

- ``default_int`` - Default integer kind (32 or 64-bit, compile-time configurable)
- ``sp``, ``dp``, ``qp`` - Single, double, and quad precision real kinds
- ``int8``, ``int16``, ``int32``, ``int64`` - Fixed-width integer kinds

Use ``default_int`` for anything that is conceptually "an integer", and an
explicit width only where the width is part of the algorithm (hash
accumulators, RNG state, serialized field widths).

Error Handling (``pic_error``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A unified error type replacing ``stat``/``errmsg`` pairs, with a cause chain
and a call-stack trace:

.. code-block:: fortran

   use pic_error
   type(error_t) :: err

   call err%set(ERROR_IO, "failed to open file")
   if (err%has_error()) call err%fatal()
   ! or with the operator form:
   if (.haserror. err) call err%fatal()

Errors can be wrapped to add context as they propagate, printing outermost
first in the style of Rust's "caused by":

.. code-block:: fortran

   call low_level_routine(err)
   if (err%has_error()) then
      call err%wrap(ERROR_PARSE, "failed to parse input")
      return
   end if

Error codes: ``SUCCESS``, ``ERROR_GENERIC``, ``ERROR_IO``, ``ERROR_PARSE``,
``ERROR_VALIDATION``, ``ERROR_ALLOC``, ``ERROR_INTERNAL``, ``ERROR_BOUNDS``.

``ERROR_VALIDATION`` means the *caller* passed something wrong;
``ERROR_INTERNAL`` means a PIC invariant was violated. That distinction is
what makes a bug report actionable, so prefer the specific code.

Codes up to ``PIC_ERROR_CODE_MAX`` (99) are reserved for PIC. Downstream
projects should use 100 and above, and can pass their own name table to
``code_to_string`` through its optional ``user_name`` argument.

.. note::

   Every ``error_t`` mutator is ``pure``, so errors can be reported from
   ``pure`` procedures — which is why the sorting routines can accept an
   optional ``err`` without giving up purity. ``fatal`` and ``print_trace``
   do I/O and are therefore impure; calling them from a ``pure`` procedure
   is a compile-time error, as it should be.

Unsigned 64-bit Arithmetic (``pic_uint64``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Arithmetic modulo 2**64 for algorithms specified in terms of wrapping 64-bit
words -- hashes, counter-based generators, fixed-point tables:

- ``u64_add``, ``u64_mul`` - wrapping add and multiply
- ``u64_shr`` - logical (zero-filling) right shift
- ``u64_less`` - unsigned comparison

Fortran has no unsigned integer type and signed overflow is undefined, so the
obvious spelling of a wrapping multiply is undefined behaviour that
``-ftrapv`` or ``-fsanitize=signed-integer-overflow`` will abort on. Every
routine splits its operands into limbs so no intermediate leaves the signed
range.

A 64-bit unsigned value is carried as the ``integer(int64)`` holding its
two's-complement pattern, so values at or above 2**63 appear negative. That is
why ``u64_less`` exists: Fortran's own ``<`` compares them as signed and
reports 2**63 as *less than* zero.

Strings (``pic_strings``, ``pic_string_type``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Comprehensive string handling:

- Dynamic string type (``string_type``)
- Conversion functions (``to_string`` from ``pic_strings``; ``to_lower``,
  ``to_upper``, ``to_title``, ``to_sentence``, ``reverse`` from ``pic_ascii``,
  and also available for ``string_type`` from ``pic_string_type``)
- String manipulation utilities
- ASCII character utilities (``pic_ascii``)

.. warning::

   The defined-I/O procedures for ``string_type`` (``read(formatted)``,
   ``write(formatted)`` and their unformatted counterparts) are **not
   available on nvfortran or classic flang** — those compilers do not
   support user-defined derived-type I/O, so the interfaces are excluded by
   preprocessor guard. Code that must build on every supported compiler
   should not rely on them.

   A ``string_type`` must also never be used as a namelist group object.

Tokenizer (``pic_tokenizer``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Splitting character data into tokens, and parsing those tokens into numbers
with explicit error reporting:

- ``split`` - split on a delimiter, **keeping** empty fields
- ``tokenize`` - split on separators, collapsing runs and dropping empties
- ``parse_int``, ``parse_real`` - checked parsing reporting through ``error_t``

The parsers deliberately avoid list-directed internal reads, which accept
almost nothing the caller meant: ``"1,2"`` reads as 1, ``"3*7"`` is a repeat
count, ``"nan"`` and ``"infinity"`` are accepted, and trailing junk after a
separator is ignored. Every accepted form is checked against an explicit
grammar first, and a malformed input yields ``ERROR_PARSE`` with a readable
message rather than a plausible wrong number.

The two split policies differ in a way that matters for columnar data:
``split`` returns *delimiter count + 1* elements always, so
``join(split(text, d), d)`` reproduces ``text`` and CSV columns stay aligned.
``tokenize`` is the right choice for whitespace-separated input.

Deterministic Formatting (``pic_format``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Number-to-string conversion producing a byte-identical result on every
supported compiler:

- ``to_string_fixed`` - fixed-point with a chosen number of decimals
- ``to_string_sci`` - scientific notation
- ``to_string_width`` - right-aligned in a field

This exists because list-directed output and the ``g0`` edit descriptor are
explicitly processor dependent — compilers disagree about digit counts,
trailing zeros, whether a leading zero appears before the decimal point, the
exponent letter, and the exponent field width. That makes golden-file testing
impossible. ``pic_format`` generates the exact decimal expansion with integer
arithmetic and rounds explicitly, round-half-to-even against the **exact
binary value**.

.. note::

   Rounding against the exact binary value is occasionally surprising:
   ``0.15_dp`` is really ``0.1499999999999999944…``, so
   ``to_string_fixed(0.15_dp, 1)`` is ``"0.1"``, not ``"0.2"``. A tie is only
   a tie for dyadic rationals such as ``0.25`` or ``2.5``. This is the only
   rounding rule that can be reproduced exactly everywhere.

Random Numbers (``pic_rng``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Reproducible pseudo-random generators with explicit state:

- ``splitmix64_t`` - SplitMix64, a 64-bit counter plus a strong finalizing mix
- ``pcg32_t`` - PCG-XSH-RR 64/32, supporting 2**63 distinct streams
- ``next_real_dp``, ``next_below``, ``stream_for`` - generic helpers

The intrinsic ``random_number`` is deliberately unspecified: every compiler
ships a different engine, a different seeding rule and a different number of
seed words, so any result depending on it cannot be regression tested across
compilers. These generators are pure functions of their state, so the same
seed yields the same bits everywhere.

``next_real_dp`` builds its value from the top 53 bits rather than dividing by
a modulus, so the result is exactly uniform on [0,1) with no rounding-to-one
edge case. ``next_below(n)`` rejects the low values that would bias a naive
modulo reduction.

Logger (``pic_logger``)
^^^^^^^^^^^^^^^^^^^^^^^

Logging utilities with multiple severity levels:

Messages are emitted through a ``logger_type`` object; ``global_logger`` is
provided ready to use.

- ``%debug``, ``%verbose``, ``%large_info``, ``%info``, ``%performance``,
  ``%warning``, ``%error``, ``%knowledge`` - emit at a level
- ``%configure(level)`` / ``%configuration(level)`` - set and query verbosity
- ``%configure_file_output(path, level)`` / ``%close_log_file`` - tee to a file
- ``%set_explicit_printing(flag)`` - show or hide the level prefix

Levels, in decreasing verbosity: ``debug_level``, ``verbose_level``,
``large_info_level``, ``info_level``, ``performance_level``, ``warning_level``,
``error_level``, ``knowledge_level``.

A pure variant (``pic_pure_logger``) provides ``pure_debug``, ``pure_info``,
``pure_warning``, ``pure_error`` and friends for use inside ``pure``
procedures. Because a pure procedure cannot do I/O, those calls append to a
buffer that impure code emits later with ``flush_log_buffer`` (or discards
with ``clear_log_buffer``).

Timer (``pic_timer``)
^^^^^^^^^^^^^^^^^^^^^

High-resolution timing utilities:

- Start/stop timing
- Elapsed time measurement
- Support for nested timers

Uses ``omp_get_wtime`` when built with OpenMP and ``system_clock`` otherwise.

.. note::

   On a processor with no clock, the standard specifies that ``system_clock``
   returns a zero count rate. ``get_elapsed_time`` reports zero elapsed time
   in that case rather than dividing by it. A caller cannot distinguish "no
   clock" from "no measurable time passed" from the result alone — both mean
   the same thing to any arithmetic downstream.

Clocks (``pic_clock``)
^^^^^^^^^^^^^^^^^^^^^^

Two unrelated notions of time, kept apart on purpose:

- ``monotonic_ms``, ``monotonic_us`` - elapsed time from an unspecified
  origin, only ever moving forward
- ``now_local``, ``now_utc``, ``datetime_t`` - calendar date and time
- ``unix_time_ms`` - milliseconds since 1970-01-01T00:00:00Z
- ``format_iso8601`` - for example ``2026-09-14T09:46:00.123Z``

Monotonic readings are for measuring how long something took, or pacing a
loop against real time; differences are meaningful, the absolute value is
not. Wall-clock time is for stamping a log line or naming a file, and can
jump backwards when the system clock is corrected, so it must never be used
to measure a duration.

Where ``pic_timer`` reports ``real(dp)`` seconds, which is what a benchmark
wants, this module reports whole milliseconds or microseconds as
``integer(int64)``, which is what a simulation pacing itself against the wall
clock wants: integers compare and accumulate exactly, so a frame budget does
not drift with rounding.

``system_clock`` is called with ``integer(int64)`` arguments, which selects a
finer tick than the default integer kind does on every supported compiler. A
processor with no clock returns ``PIC_CLOCK_NO_CLOCK`` (-1) rather than zero,
since zero is a perfectly valid reading.

The calendar conversions are integer-only and exact for every date in the
``int64`` range, including the full Gregorian leap rule. ``format_iso8601``
builds its text with ``zfill`` rather than an internal ``write``, because the
``I0.N`` edit descriptor and list-directed output are processor dependent and
this text is compared byte for byte.

.. note::

   Nothing here is reproducible between runs, by definition. Code whose
   results must replay identically from a seed must not call it.
   ``format_iso8601`` and ``unix_time_ms`` are the exceptions: both are pure
   functions of their arguments.

Profiler (``pic_profiler``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Named, stack-based code regions with optional NVTX support:

.. code-block:: fortran

   use pic_profiler

   call profiler_init()
   call profiler_start("outer")
   call profiler_start("inner")
   ! ...
   call profiler_stop()   ! stops "inner" - the stack decides, not the name
   call profiler_stop()   ! stops "outer"
   call profiler_report()
   call profiler_finalize()

Build with ``-DPIC_USE_NVTX=ON`` for NVIDIA Nsight Systems integration, or
``-DPIC_DISABLE_PROFILER=ON`` for zero overhead.

Arrays (``pic_array``)
^^^^^^^^^^^^^^^^^^^^^^

Array utilities:

- ``pic_fill`` - fill 1-D or 2-D arrays, generic over int32/int64/sp/dp
- ``pic_copy``, ``pic_sum``, ``pic_transpose``
- ``pic_scramble_array`` - shuffle, useful for building sort test cases
- ``pic_print_array`` - NUMPY, MATHEMATICA and PLAIN output formats
- ``is_sorted`` - with ``ASCENDING`` / ``DESCENDING``
- ``set_threading_mode`` / ``get_threading_mode`` - default OpenMP threading,
  overridable per call with the ``threaded`` argument

Fixed-Capacity Arrays (``pic_fixed_array``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Bounded containers with push/pop/size semantics and no heap allocation:

- ``fixed_array_int_t``, ``fixed_array_dp_t``
- Capacity fixed at compile time by ``PIC_FIXED_ARRAY_CAPACITY``

The Fortran analogue of C++'s ``std::inplace_vector``. Element storage is a
fixed-size component — never ``allocatable``, never a ``pointer`` — so an
instance can live on the stack, inside a ``block`` construct, or as a local of
an OpenMP/OpenACC region without touching the heap. Every failure mode
(capacity overflow, popping empty, out-of-range index) reports
``ERROR_VALIDATION`` rather than writing past the end.

.. note::

   A parameterized derived type with a ``len`` parameter would be the textbook
   way to make capacity per-instance. It is deliberately not used: PDT support
   is poor or absent on several of the compilers PIC exists to support.

   The ``err`` argument here is ``intent(inout)``, not ``intent(out)``, and is
   written only on failure. An ``intent(out)`` ``error_t`` would oblige the
   compiler to deallocate its ``message`` component on entry to *every* call,
   putting heap traffic back into the hot path of a container whose whole
   purpose is not having any.

Growable Vectors (``pic_vector``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Heap-backed arrays that grow on demand:

- ``vector_int32_t``, ``vector_int64_t``, ``vector_dp_t``, ``vector_string_t``
- ``push_back``, ``append``, ``pop_back``, ``at``, ``set``, ``get_unchecked``
- ``size``, ``capacity``, ``is_empty``, ``reserve``, ``resize``, ``clear``
- ``shrink_to_fit``, ``as_array``, ``take``, ``destroy``

The method names deliberately match ``pic_fixed_array``, so moving from a
bounded container to a growable one is a type change and nothing else. Choose
``pic_fixed_array`` in hot loops and inside OpenMP/OpenACC regions, where its
storage lives inside the object and never touches the heap; choose
``pic_vector`` when the final length is not known until the input has been
read.

``take`` is the reason this is not just "an array you resize yourself": it
hands the backing storage to an ``allocatable`` array with ``move_alloc``,
exactly sized, without copying the elements. ``as_array`` copies; ``take``
does not.

.. code-block:: fortran

   use pic_vector, only: vector_int32_t
   use pic_types, only: int32
   use pic_error, only: error_t

   type(vector_int32_t) :: v
   type(error_t) :: err
   integer(int32), allocatable :: final(:)

   call v%push_back(42_int32, err)
   call v%append([7_int32, 9_int32], err)
   call v%take(final, err)       ! v is empty; `final` has exactly 3 elements

Element kinds are fixed width. There is no ``vector_int_t`` following
``default_int``, because the same source would mean a 32-bit container in one
build and a 64-bit one in the other. Sizes and indices are ``default_int``.

The backing storage is **private**. Only ``1:size()`` is meaningful, and a
public component would let ``v%items(v%size() + 1)`` compile and read spare
capacity. The two cases that genuinely need to avoid a copy are served
directly by ``take`` and ``get_unchecked``.

Every operation is ``pure``, so a vector can be built and consumed inside a
``pure`` procedure. Failures report through ``error_t``: ``ERROR_ALLOC`` when
storage cannot be grown, ``ERROR_BOUNDS`` for an out-of-range index or a pop
from an empty vector, ``ERROR_VALIDATION`` for a negative ``resize``.

Sorting (``pic_sorting``)
^^^^^^^^^^^^^^^^^^^^^^^^^

Sorting algorithms that work across all compilers:

- ``sort`` - introsort with a heapsort fallback
- ``ord_sort`` - a stable merge sort
- ``radix_sort`` - for integer and real keys
- ``sort_index`` - produces a permutation index rather than reordering in place

``sort`` and ``ord_sort`` are ``pure``, so they can be called from your own
``pure`` procedures.

``ord_sort``, ``sort_index`` and ``radix_sort`` accept an **optional** ``err``
argument of type ``error_t`` — each can need a scratch buffer, so each has
something that can fail. When ``err`` is absent an unrecoverable condition
still stops the program via ``error stop``, because a pure procedure has no
other way to report. Passing ``err`` is purely additive and changes no
existing call.

``sort`` takes no ``err``: introsort works in place and allocates nothing, so
there is no failure for it to report.

.. note::

   ``sort_index(..., reverse=.true.)`` reverses ``array`` in place *before*
   sorting. If it then fails, ``array`` is left reversed — every other failure
   mode leaves the input untouched. This is the one case where a recovering
   caller cannot assume its input survived.

Heap / Priority Queue (``pic_heap``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A binary heap over ``dp`` values:

- ``init``, ``clear``, ``destroy``, ``reserve``
- ``push``, ``pop``, ``peek``
- ``build_from`` - Floyd's O(n) heapify, not n successive pushes
- ``size``, ``is_empty``, ``capacity``

``clear`` keeps the allocated storage and resets the count; ``destroy``
releases it. A priority queue reused across iterations of an outer loop should
use ``clear`` and never reallocate.

Min or max ordering is chosen at ``init``, rather than by a comparator
procedure pointer — procedure-pointer components in derived types are a
recurring portability problem across this compiler matrix.

Hash Functions (``pic_hash_32bit``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

32-bit hash functions:

- FNV-1a hash implementation
- General-purpose hashing utilities

Hash Map (``pic_hash_map``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A string-keyed map with a **guaranteed iteration order**:

- ``insert``, ``get``, ``has_key``, ``remove``, ``at``
- ``keys``, ``values`` - always returned in **insertion order**
- ``size``, ``is_empty``, ``bucket_count``

The ordering guarantee is a documented promise, not an accident of the bucket
layout. A map that iterates in bucket order produces output that changes when
the hash changes, when the capacity changes, or when a compiler's integer
arithmetic differs — which makes any report or golden file built on top of it
untestable. ``remove`` preserves the relative order of what remains.

Keys are deferred-length character, so there is no key-length limit.

Array State Hashing (``pic_array_hash``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A stable digest of an array's contents, for answering "did this run produce
the same state as that run" without diffing gigabytes. Covers the intrinsic
types and both real kinds, ranks 1 to 3.

.. note::

   ``transfer`` between real and integer kinds is **not** used, even though it
   is the obvious way to reach a real's bits: nvfortran and LFortran disagree
   with GNU and Intel about the result for some inputs, which is fatal for a
   hash whose entire purpose is cross-compiler comparison. The real paths
   decompose values with ``fraction`` and ``exponent`` instead.

   Two special cases are handled explicitly: ``-0.0`` hashes identically to
   ``+0.0`` (they compare equal, so a hash that distinguished them would
   report a difference where ``==`` reports none), and NaN is normalized to a
   single canonical pattern so that two runs which both produced NaN do not
   hash differently.

Sparse Matrices (``pic_csr``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Compressed sparse row storage:

- ``build_from_coo`` - accepts unsorted triplets and sums duplicates
- ``matvec``, ``transpose`` - both ``pure``
- ``row_slice`` - index range for a row, so callers can iterate without copying
- ``n_rows``, ``n_cols``, ``nnz``, ``is_valid``, ``destroy``

``build_from_coo`` accepting duplicates matters because that is what finite
element assembly actually produces. ``transpose`` is the O(nnz) counting-sort
construction, not a round trip through COO.

Graphs (``pic_graph``)
^^^^^^^^^^^^^^^^^^^^^^

Shortest paths and connectivity over CSR adjacency:

- ``dijkstra`` - weighted shortest paths, using ``pic_heap``
- ``a_star`` - with a per-node heuristic supplied as an array
- ``bfs`` - the unweighted case, without the cost of a heap
- ``connected_components``
- ``path_cost`` - reconstructs and re-sums a path from the predecessor array

Constants: ``GRAPH_INFINITY``, ``GRAPH_NO_PREDECESSOR``, ``GRAPH_UNREACHABLE``.
"Unreachable" is a named constant rather than a magic number, so "no path" is
never confused with "distance zero".

``dijkstra`` reports ``ERROR_VALIDATION`` on a negative edge weight rather
than returning a plausible wrong answer. The algorithm cannot detect this
mid-run, so it is checked up front.

Serialization (``pic_serialize``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Binary serialization with a self-describing envelope recording magic number,
format version, element kind, rank and extents.

Unformatted **stream** access is used rather than unformatted sequential
writes, because sequential record markers are processor dependent in both
width and endianness — a file written by gfortran is not readable by ifx. The
envelope is checked on read, and every mismatch (wrong magic, a version from
the future, a kind mismatch, a rank or extent mismatch, a truncated payload)
is a distinct ``error_t`` naming what was expected and what was found, rather
than a partly-filled array.

I/O errors carry the processor's own ``iostat`` value in the message text.

Struct-of-Arrays (``pic_soa``, ``pic_soa_particle``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Struct-of-arrays containers generated from an fypp template, with amortized
``resize``, ``checkpoint`` through ``pic_serialize``, and ``state_hash``
through ``pic_array_hash``.

Fields are parallel arrays sharing a size and capacity, so each field is
contiguous and can be handed to BLAS, MPI or a GPU kernel without a gather.
A live slice such as ``p%x(1:p%size())`` reaches a callee with its contiguity
intact rather than being copied.

To generate a container for your own particle type, edit
``tools/autogen/pic_soa.fypp`` and regenerate — see :doc:`contributing`.

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

Lore, akin to Fortune.

Optional Features
-----------------

These features require additional dependencies or compiler flags:

OpenMP Support
^^^^^^^^^^^^^^

Enable with ``-DPIC_ENABLE_OMP=ON``. Provides parallel implementations of
various operations.

NVTX Annotations
^^^^^^^^^^^^^^^^

Enable with ``-DPIC_USE_NVTX=ON`` to have ``pic_profiler`` regions appear in
NVIDIA Nsight Systems timelines.
