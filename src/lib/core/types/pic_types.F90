!! pic_types.F90 controls the standarized sizes for the datatypes across
!! pic, this is key for interfacing with other codes specially those that
!! use default sizes

module pic_types
   !! main module for defining types for integer and double precision
   use, intrinsic :: iso_fortran_env, only: int32, int64
   implicit none

   private

   public :: int32, int64
   ! Define kinds for different data types
   ! int32 and int64 are defined in the iso_fortran_env, if you need to change things please do so here
   integer, parameter, public :: sp = SELECTED_REAL_KIND(6, 37)
      !! single precision size
   integer, parameter, public :: dp = SELECTED_REAL_KIND(15, 307)
      !! double precision size
   integer, parameter, public :: qp = SELECTED_REAL_KIND(33, 4931)
      !! quadruple precision size, varies by compiler

   ! Define default types
#ifdef USE_INT8
   integer, parameter, public :: default_int = int64
    !! if you compile PIC requesting USE_INT8 the default_int will be set to int64 this is kinda equivalent
    !! to compiling with -i8. If linking to a legacy codebase that relies on this, compile PIC with USE_INT8
#else
   integer, parameter, public :: default_int = int32
    !! the default integer kind in PIC is int32 which faciliates the interfaces to MPI
    !! pay special attention if linking PIC to a code that use default int size of 8
#endif
   integer, parameter, public :: default_real = dp
    !! naturally, our default real is double precision
   integer, parameter, public :: default_complex = dp
    !! default complex is double precision

   integer, parameter, public :: int_index = int64  !! Integer kind for indexing
   integer, parameter, public :: int_index_low = int32  !! Integer kind for indexing using less than `huge(1_int32)` values

end module pic_types
