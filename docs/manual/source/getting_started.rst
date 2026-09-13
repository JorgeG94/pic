Getting Started
===============

This guide will help you get started with PIC in your Fortran projects.

Basic Usage
-----------

The ``pic`` module itself only provides the banner, which is handy for
verifying an install:

.. code-block:: fortran

   program my_program
      use pic, only: pic_print_banner
      implicit none

      call pic_print_banner()
   end program my_program

Everything else is imported from the module that provides it. There is no
umbrella module re-exporting the whole library — importing only what you use
keeps compile times down and makes the dependency explicit:

.. code-block:: fortran

   program my_program
      use pic_types, only: default_int, dp
      use pic_strings, only: to_string
      use pic_logger, only: global_logger
      implicit none

      ! Your code here
   end program my_program

Types and Kinds
---------------

PIC uses portable kind definitions. **Always use these instead of literal kinds** to ensure your code compiles on all compilers.

.. code-block:: fortran

   use pic_types, only: default_int, sp, dp, qp, int32, int64

   ! Portable integer (32 or 64-bit depending on build config)
   integer(default_int) :: n

   ! Fixed-width integers
   integer(int32) :: i32
   integer(int64) :: i64

   ! Floating point kinds
   real(sp) :: single_val    ! Single precision (32-bit)
   real(dp) :: double_val    ! Double precision (64-bit)
   real(qp) :: quad_val      ! Quad precision (128-bit)

.. warning::

   Never use bare ``integer`` or ``integer(4)`` / ``integer(8)`` / ``real(8)``.
   These lead to portability issues across compilers. Always use PIC's kind definitions.

Why Default Integer Matters
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many legacy Fortran codes compile with flags like ``-fdefault-integer-8`` to make all integers 64-bit. This causes problems when interfacing with other codes.

PIC solves this by providing ``default_int`` which can be toggled at **compile time** without compiler flags:

.. code-block:: bash

   # Build with 32-bit default integers (default)
   cmake -B build

   # Build with 64-bit default integers
   cmake -B build -DPIC_DEFAULT_INT8=ON

String Operations
-----------------

PIC provides a dynamic string type and string utilities:

.. code-block:: fortran

   use pic_string_type, only: string_type
   use pic_strings, only: to_string, starts_with, ends_with
   use pic_ascii, only: to_lower, to_upper
   use pic_types, only: dp

   type(string_type) :: s
   character(len=:), allocatable :: str

   ! Convert numbers to strings
   str = to_string(42)           ! "42"
   str = to_string(3.14159_dp)   ! "3.14159..."
   str = to_string(.true.)       ! "T"

   ! Case conversion
   str = to_lower("HELLO WORLD")  ! "hello world"
   str = to_upper("hello world")  ! "HELLO WORLD"

   ! String queries
   if (starts_with("hello.f90", "hello")) then
      print *, "Starts with hello"
   end if

Logging
-------

PIC includes a logging system with severity levels:

.. code-block:: fortran

   use pic_logger, only: global_logger, debug_level

   ! Messages go through a logger object; `global_logger` is provided
   call global_logger%debug("Detailed debug information")
   call global_logger%info("Starting computation...")
   call global_logger%warning("Memory usage is high")
   call global_logger%error("Failed to open file")

   ! An optional second argument names the emitting module
   call global_logger%info("Cache warm", "my_module")

   ! Raise verbosity to see debug messages
   call global_logger%configure(debug_level)

Output example::

   [INFO] Starting computation...
   [WARNING] Memory usage is high
   [ERROR] Failed to open file

Pure Logger
^^^^^^^^^^^

For use in ``pure`` procedures, use ``pic_pure_logger``:

.. code-block:: fortran

   use pic_pure_logger, only: pure_info, flush_log_buffer

   pure function compute(x) result(y)
      real(dp), intent(in) :: x
      real(dp) :: y

      ! Buffered; nothing is written until the buffer is flushed
      call pure_info("Computing...")
      y = x*2.0_dp
   end function compute

Because a ``pure`` procedure may not perform I/O, messages are appended to a
buffer. Emit them from impure code once you are back outside:

.. code-block:: fortran

   call flush_log_buffer()

Error Handling
--------------

PIC reports failures through a single ``error_t`` type rather than
``stat``/``errmsg`` pairs:

.. code-block:: fortran

   use pic_error

   type(error_t) :: err

   call err%set(ERROR_IO, "failed to open file")

   if (err%has_error()) then
      call err%print_trace()   ! print and carry on
      call err%fatal()         ! or print and stop
   end if

Add context as an error travels back up, and the trace prints outermost
first, with the root cause last:

.. code-block:: fortran

   call parse_config(path, err)
   if (err%has_error()) then
      call err%wrap(ERROR_PARSE, "could not load configuration")
      return
   end if

   ! ERROR_PARSE: could not load configuration
   !   Caused by: ERROR_IO: failed to open file

Check for a specific code with ``err%is(ERROR_IO)``, and clear a handled
error with ``err%clear()``.

.. note::

   Every mutator is ``pure``, so errors can be raised from ``pure``
   procedures — that is what lets the sorting routines take an optional
   ``err`` without giving up purity. ``fatal`` and ``print_trace`` do I/O and
   are impure; calling them from a ``pure`` procedure is a compile-time error.

Timer
-----

Measure execution time with high-resolution timers:

.. code-block:: fortran

   use pic_timer
   use pic_types, only: dp

   type(timer_type) :: t
   real(dp) :: elapsed

   call t%start()

   ! ... your computation ...

   call t%stop()
   elapsed = t%get_elapsed_time()

   print '(A,F10.3,A)', "Elapsed time: ", elapsed, " seconds"

Array Operations
----------------

PIC provides array utilities with optional OpenMP parallelization:

.. code-block:: fortran

   use pic_array, only: pic_fill, pic_copy, pic_sum, is_sorted, &
                        set_threading_mode
   use pic_types, only: dp

   real(dp) :: vec(1000), other(1000)
   real(dp) :: mat(100, 100)
   real(dp) :: total

   ! One generic covers 1-D and 2-D, and int32/int64/sp/dp
   call pic_fill(vec, 0.0_dp)
   call pic_fill(mat, 1.0_dp)

   call pic_copy(vec, other)
   total = pic_sum(vec)

   if (is_sorted(vec)) print *, "already ordered"

   ! Threading (requires -DPIC_ENABLE_OMP=ON) can be set per call...
   call pic_fill(vec, 0.0_dp, threaded=.true.)

   ! ...or as the default for subsequent calls
   call set_threading_mode(.true.)

Sorting
-------

Sorting routines that work on all compilers (unlike stdlib which may fail on some):

.. code-block:: fortran

   use pic_sorting, only: sort, ord_sort, sort_index
   use pic_types, only: dp, int_index

   real(dp) :: arr(100)
   integer(int_index) :: indices(100)

   ! Sort in place (introsort)
   call sort(arr)

   ! Stable merge sort
   call ord_sort(arr)

   ! Sort and also return the permutation that produced it.
   ! `index` is integer(int_index), not default_int.
   call sort_index(arr, indices)

``sort`` and ``ord_sort`` are ``pure``, so they can be called from your own
``pure`` procedures.

``ord_sort``, ``sort_index`` and ``radix_sort`` take an **optional** ``err`` of
type ``error_t``, because each of them can need a scratch buffer and therefore
has something that can fail:

.. code-block:: fortran

   use pic_sorting, only: ord_sort
   use pic_error, only: error_t
   use pic_types, only: dp

   real(dp) :: arr(100)
   type(error_t) :: err

   call ord_sort(arr, err=err)
   if (err%has_error()) call err%fatal()

``sort`` has no ``err`` argument and needs none — introsort works in place and
allocates nothing.

Hash Functions
--------------

FNV-1a 32-bit hash implementation:

.. code-block:: fortran

   use pic_hash_32bit, only: fnv_1a_hash
   use pic_types, only: int32

   integer(int32) :: hash_val
   character(len=*), parameter :: key = "my_key"

   hash_val = fnv_1a_hash(key)

``fnv_1_hash`` is also available if you need FNV-1 rather than FNV-1a.

Complete Example
----------------

Here's a complete example showing multiple PIC features:

.. code-block:: fortran

   program pic_demo
      use pic_types, only: default_int, dp
      use pic_timer, only: timer_type
      use pic_array, only: pic_fill, is_sorted
      use pic_sorting, only: ord_sort
      use pic_logger, only: global_logger
      use pic_strings, only: to_string
      use pic_error, only: error_t
      implicit none

      type(timer_type) :: timer
      type(error_t) :: err
      real(dp) :: values(1000), elapsed
      integer(default_int) :: i

      call timer%start()

      ! Fill, then overwrite with a descending ramp so there is work to do
      call pic_fill(values, 0.0_dp)
      do i = 1, size(values)
         values(i) = real(size(values) - i, dp)
      end do

      call ord_sort(values, err=err)
      if (err%has_error()) call err%fatal()

      call timer%stop()
      elapsed = timer%get_elapsed_time()

      call global_logger%info("Sorted "//to_string(size(values))//" elements")
      call global_logger%info("Sorted correctly: "//to_string(is_sorted(values)))
      call global_logger%info("Time: "//to_string(elapsed)//" seconds")

   end program pic_demo

Next Steps
----------

- Explore the :doc:`features` page for a complete module reference
- Check the `API documentation <https://jorgeg94.github.io/pic/>`_ for detailed interfaces
- See the `examples repository <https://github.com/JorgeG94/pic_examples>`_ for more use cases
