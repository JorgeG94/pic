! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! pic_types.F90 controls the standarized sizes for the datatypes across
!! pic, this is key for interfacing with other codes specially those that
!! use default sizes

module pic_types
   !! main module for defining types for integer and double precision
   use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
#ifdef PIC_C_COMPAT
   use, intrinsic :: iso_c_binding, only: c_bool, c_char, &
      c_float, c_double, c_long_double, c_int, c_int64_t
#else
   use, intrinsic :: iso_c_binding, only: c_bool, c_char
#endif
   implicit none

   private

   public :: int8, int16, int32, int64
   public :: c_bool, c_char
   ! Define kinds for different data types
   ! int32 and int64 are defined in the iso_fortran_env, if you need to change things please do so here
#ifdef PIC_C_COMPAT
   integer, parameter, public :: sp = c_float
      !! single precision size (C_FLOAT via iso_c_binding)
   integer, parameter, public :: dp = c_double
      !! double precision size (C_DOUBLE via iso_c_binding)
   integer, parameter, public :: qp = c_long_double
      !! quadruple precision size (C_LONG_DOUBLE via iso_c_binding)
#else
   integer, parameter, public :: sp = SELECTED_REAL_KIND(6, 37)
      !! single precision size
   integer, parameter, public :: dp = SELECTED_REAL_KIND(15, 307)
      !! double precision size
   integer, parameter, public :: qp = SELECTED_REAL_KIND(33, 4931)
      !! quadruple precision size, varies by compiler
#endif

   ! Define default types
#ifdef USE_INT8
   integer, parameter, public :: default_int = int64
     !! if you compile PIC requesting USE_INT8 the default_int will be set to int64 this is kinda equivalent
     !! to compiling with -i8. If linking to a legacy codebase that relies on this, compile PIC with USE_INT8
#elif defined(PIC_C_COMPAT)
   integer, parameter, public :: default_int = c_int
     !! when PIC_C_COMPAT is defined, default_int matches C int (typically int32)
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

   integer, parameter, public :: fbool = kind(.true.)

end module pic_types
