# 1 "/home/runner/work/pic/pic/src/lib/core/types/pic_types.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/runner/work/pic/pic/build_ccompat//"
# 1 "/home/runner/work/pic/pic/src/lib/core/types/pic_types.F90"
! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! pic_types.F90 controls the standarized sizes for the datatypes across
!! pic, this is key for interfacing with other codes specially those that
!! use default sizes

module pic_types
   !! main module for defining types for integer and double precision
   use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64

   use, intrinsic :: iso_c_binding, only: c_bool, c_char, &
      c_float, c_double, c_long_double, c_int, c_int64_t



   implicit none

   private

   public :: int8, int16, int32, int64
   public :: c_bool, c_char
   ! Define kinds for different data types
   ! int32 and int64 are defined in the iso_fortran_env, if you need to change things please do so here

   integer, parameter, public :: sp = c_float
      !! single precision size (C_FLOAT via iso_c_binding)
   integer, parameter, public :: dp = c_double
      !! double precision size (C_DOUBLE via iso_c_binding)
   integer, parameter, public :: qp = c_long_double
      !! quadruple precision size (C_LONG_DOUBLE via iso_c_binding)
# 39 "/home/runner/work/pic/pic/src/lib/core/types/pic_types.F90"

   ! Define default types





   integer, parameter, public :: default_int = c_int
     !! when 1 is defined, default_int matches C int (typically int32)





   integer, parameter, public :: default_real = dp
     !! naturally, our default real is double precision
   integer, parameter, public :: default_complex = dp
     !! default complex is double precision

   integer, parameter, public :: int_index = int64  !! Integer kind for indexing
   integer, parameter, public :: int_index_low = int32  !! Integer kind for indexing using less than `huge(1_int32)` values

   integer, parameter, public :: fbool = kind(.true.)

end module pic_types
