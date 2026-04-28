Getting Started
===============

This guide will help you get started with PIC in your Fortran projects.

Basic Usage
-----------

PIC provides an umbrella module that exposes all functionality:

.. code-block:: fortran

   program my_program
      use pic
      implicit none

      ! Your code here
   end program my_program

For finer control, import specific modules:

.. code-block:: fortran

   program my_program
      use pic_types, only: default_int, dp
      use pic_strings, only: to_string
      use pic_logger, only: log_info
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
   use pic_strings, only: to_string, to_lower, to_upper, starts_with, ends_with

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

   use pic_logger

   ! Different severity levels
   call log_debug("Detailed debug information")
   call log_info("Starting computation...")
   call log_warning("Memory usage is high")
   call log_error("Failed to open file")

Output example::

   [INFO] Starting computation...
   [WARNING] Memory usage is high
   [ERROR] Failed to open file

Pure Logger
^^^^^^^^^^^

For use in ``pure`` procedures, use ``pic_pure_logger``:

.. code-block:: fortran

   use pic_pure_logger

   pure function compute(x) result(y)
      real(dp), intent(in) :: x
      real(dp) :: y
      character(len=256) :: msg

      ! Pure logging (deferred output)
      call pure_log_info("Computing...", msg)
      y = x * 2.0_dp
   end function compute

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
   elapsed = t%elapsed()

   print '(A,F10.3,A)', "Elapsed time: ", elapsed, " seconds"

Array Operations
----------------

PIC provides array utilities with optional OpenMP parallelization:

.. code-block:: fortran

   use pic_array, only: fill_vector, fill_matrix
   use pic_types, only: dp

   real(dp) :: vec(1000)
   real(dp) :: mat(100, 100)

   ! Fill arrays with values
   call fill_vector(vec, 0.0_dp)
   call fill_matrix(mat, 1.0_dp)

   ! Enable threaded filling (requires PIC_ENABLE_OMP)
   call fill_vector(vec, 0.0_dp, threaded=.true.)

Sorting
-------

Sorting routines that work on all compilers (unlike stdlib which may fail on some):

.. code-block:: fortran

   use pic_sorting
   use pic_types, only: dp

   real(dp) :: arr(100)
   integer(default_int) :: indices(100)

   ! Sort in place
   call sort(arr)

   ! Get sorted indices
   call argsort(arr, indices)

Hash Functions
--------------

FNV-1a 32-bit hash implementation:

.. code-block:: fortran

   use pic_hash_32bit, only: fnv1a_hash
   use pic_types, only: int32

   integer(int32) :: hash_val
   character(len=*), parameter :: key = "my_key"

   hash_val = fnv1a_hash(key)

Complete Example
----------------

Here's a complete example showing multiple PIC features:

.. code-block:: fortran

   program pic_demo
      use pic
      implicit none

      type(timer_type) :: timer
      real(dp) :: data(1000), elapsed
      integer(default_int) :: i

      ! Start timing
      call timer%start()

      ! Initialize array
      call fill_vector(data, 0.0_dp)
      do i = 1, size(data)
         data(i) = real(i, dp)
      end do

      ! Sort the data
      call sort(data)

      ! Stop timing
      call timer%stop()
      elapsed = timer%elapsed()

      ! Log results
      call log_info("Sorted " // to_string(size(data)) // " elements")
      call log_info("Time: " // to_string(elapsed) // " seconds")

   end program pic_demo

Next Steps
----------

- Explore the :doc:`features` page for a complete module reference
- Check the `API documentation <https://jorgeg94.github.io/pic/>`_ for detailed interfaces
- See the `examples repository <https://github.com/JorgeG94/pic_examples>`_ for more use cases
