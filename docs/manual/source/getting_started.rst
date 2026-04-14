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

Alternatively, you can import specific modules:

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

PIC uses portable kind definitions. Always use these instead of literal kinds:

.. code-block:: fortran

   use pic_types, only: default_int, sp, dp, qp

   integer(default_int) :: my_int      ! Portable integer (32 or 64-bit)
   real(sp) :: single_precision        ! Single precision real
   real(dp) :: double_precision        ! Double precision real
   real(qp) :: quad_precision          ! Quad precision real

.. warning::

   Never use bare ``integer`` or ``integer(4)`` / ``integer(8)``.
   Always use ``integer(default_int)`` for portability.

String Operations
-----------------

PIC provides a string type and various string utilities:

.. code-block:: fortran

   use pic_string_type, only: string_type
   use pic_strings, only: to_string, to_lower, to_upper

   type(string_type) :: s
   character(len=:), allocatable :: str

   ! Convert numbers to strings
   str = to_string(42)
   str = to_string(3.14159_dp)

   ! Case conversion
   str = to_lower("HELLO")  ! "hello"
   str = to_upper("world")  ! "WORLD"

Logging
-------

PIC includes a logging system:

.. code-block:: fortran

   use pic_logger

   call log_info("Starting computation")
   call log_warning("This might take a while")
   call log_error("Something went wrong")

Timer
-----

Measure execution time with the timer module:

.. code-block:: fortran

   use pic_timer

   type(timer_type) :: t

   call t%start()
   ! ... your computation ...
   call t%stop()

   print *, "Elapsed time:", t%elapsed(), "seconds"

Arrays
------

PIC provides array utilities including filling and operations:

.. code-block:: fortran

   use pic_array, only: fill_vector

   real(dp) :: vec(1000)

   call fill_vector(vec, 0.0_dp)  ! Fill with zeros
