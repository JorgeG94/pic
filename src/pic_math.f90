! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Scalar numeric utility functions: range clamping, range checking, and
!! approximate floating-point equality.
module pic_math
   !! This module provides small, elemental numeric helpers that are commonly
   !! needed but missing from the Fortran intrinsics: `clamp`, `is_in_range`
   !! and `is_close`. All procedures are `elemental`, so they apply equally to
   !! scalars and to arrays of any rank/shape.
   use pic_types, only: int32, int64, sp, dp
   implicit none

   private

   public :: clamp, is_in_range, is_close

   interface clamp
   !! Constrain a value to the inclusive range `[lo, hi]`.
   !!
   !! Returns `lo` if `x < lo`, `hi` if `x > hi`, otherwise `x`. Supported
   !! types are: integer(int32), integer(int64), real(sp), real(dp).
   !! Usage: `y = clamp(x, lo, hi)`
      module procedure :: clamp_int32
      module procedure :: clamp_int64
      module procedure :: clamp_sp
      module procedure :: clamp_dp
   end interface clamp

   interface is_in_range
   !! Test whether a value lies within the inclusive range `[lo, hi]`.
   !!
   !! Returns `.true.` when `lo <= x .and. x <= hi`. Supported types are:
   !! integer(int32), integer(int64), real(sp), real(dp).
   !! Usage: `flag = is_in_range(x, lo, hi)`
      module procedure :: is_in_range_int32
      module procedure :: is_in_range_int64
      module procedure :: is_in_range_sp
      module procedure :: is_in_range_dp
   end interface is_in_range

   interface is_close
   !! Test two floating-point values for approximate equality.
   !!
   !! Returns `.true.` when
   !! `abs(a - b) <= max(rtol*max(abs(a), abs(b)), atol)`.
   !! The relative tolerance `rtol` defaults to `sqrt(epsilon)` for the
   !! working kind and the absolute tolerance `atol` defaults to `0`; both
   !! are optional. Supported kinds are real(sp) and real(dp).
   !! Usage: `flag = is_close(a, b [, rtol] [, atol])`
      module procedure :: is_close_sp
      module procedure :: is_close_dp
   end interface is_close

contains

   elemental function clamp_int32(x, lo, hi) result(y)
      !! Clamp an integer(int32) value to `[lo, hi]`.
      integer(int32), intent(in) :: x, lo, hi
      integer(int32) :: y
      y = min(max(x, lo), hi)
   end function clamp_int32

   elemental function clamp_int64(x, lo, hi) result(y)
      !! Clamp an integer(int64) value to `[lo, hi]`.
      integer(int64), intent(in) :: x, lo, hi
      integer(int64) :: y
      y = min(max(x, lo), hi)
   end function clamp_int64

   elemental function clamp_sp(x, lo, hi) result(y)
      !! Clamp a real(sp) value to `[lo, hi]`.
      real(sp), intent(in) :: x, lo, hi
      real(sp) :: y
      y = min(max(x, lo), hi)
   end function clamp_sp

   elemental function clamp_dp(x, lo, hi) result(y)
      !! Clamp a real(dp) value to `[lo, hi]`.
      real(dp), intent(in) :: x, lo, hi
      real(dp) :: y
      y = min(max(x, lo), hi)
   end function clamp_dp

   elemental function is_in_range_int32(x, lo, hi) result(flag)
      !! Inclusive range test for integer(int32).
      integer(int32), intent(in) :: x, lo, hi
      logical :: flag
      flag = (lo <= x) .and. (x <= hi)
   end function is_in_range_int32

   elemental function is_in_range_int64(x, lo, hi) result(flag)
      !! Inclusive range test for integer(int64).
      integer(int64), intent(in) :: x, lo, hi
      logical :: flag
      flag = (lo <= x) .and. (x <= hi)
   end function is_in_range_int64

   elemental function is_in_range_sp(x, lo, hi) result(flag)
      !! Inclusive range test for real(sp).
      real(sp), intent(in) :: x, lo, hi
      logical :: flag
      flag = (lo <= x) .and. (x <= hi)
   end function is_in_range_sp

   elemental function is_in_range_dp(x, lo, hi) result(flag)
      !! Inclusive range test for real(dp).
      real(dp), intent(in) :: x, lo, hi
      logical :: flag
      flag = (lo <= x) .and. (x <= hi)
   end function is_in_range_dp

   elemental function is_close_sp(a, b, rtol, atol) result(flag)
      !! Approximate equality for real(sp) values.
      real(sp), intent(in) :: a, b
      real(sp), intent(in), optional :: rtol, atol
      logical :: flag
      real(sp) :: rel, abst

      rel = sqrt(epsilon(a))
      abst = 0.0_sp
      if (present(rtol)) rel = rtol
      if (present(atol)) abst = atol

      flag = abs(a - b) <= max(rel*max(abs(a), abs(b)), abst)
   end function is_close_sp

   elemental function is_close_dp(a, b, rtol, atol) result(flag)
      !! Approximate equality for real(dp) values.
      real(dp), intent(in) :: a, b
      real(dp), intent(in), optional :: rtol, atol
      logical :: flag
      real(dp) :: rel, abst

      rel = sqrt(epsilon(a))
      abst = 0.0_dp
      if (present(rtol)) rel = rtol
      if (present(atol)) abst = atol

      flag = abs(a - b) <= max(rel*max(abs(a), abs(b)), abst)
   end function is_close_dp

end module pic_math
