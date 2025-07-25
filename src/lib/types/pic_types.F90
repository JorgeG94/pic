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
      !! single precision variable
   integer, parameter, public :: dp = SELECTED_REAL_KIND(15, 307)
      !! double precision variable
   integer, parameter, public :: qp = SELECTED_REAL_KIND(33, 4931)
      !! quadruple precision variable

   ! Define default types
#ifdef USE_INT8
   integer, parameter, public :: default_int = int64
#else
   integer, parameter, public :: default_int = int32
#endif
    !! default integer kind, be careful if you are using fdefault-size=8
   integer, parameter, public :: default_real = dp
    !! naturally, our default real is double precision
   integer, parameter, public :: default_complex = dp
    !! default complex is double precision

end module pic_types
