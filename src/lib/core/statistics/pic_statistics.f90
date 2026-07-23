! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Basic descriptive statistics for 1-D arrays: mean, variance, standard
!! deviation and combined min/max.
module pic_statistics
   !! This module provides simple reductions over rank-1 arrays. To avoid
   !! precision loss and integer overflow, `mean`, `variance` and `std_dev`
   !! always accumulate in and return real(dp), regardless of the input type.
   !! `min_max` preserves the input type and returns the two extremes in a
   !! length-2 array.
   use pic_types, only: int32, int64, sp, dp, default_int
   implicit none

   private

   public :: mean, variance, std_dev, min_max

   interface mean
   !! Arithmetic mean of a 1-D array, returned as real(dp).
   !!
   !! Returns `0` for an empty array. Supported input types are:
   !! integer(int32), integer(int64), real(sp), real(dp).
   !! Usage: `m = mean(array)`
      module procedure :: mean_int32
      module procedure :: mean_int64
      module procedure :: mean_sp
      module procedure :: mean_dp
   end interface mean

   interface variance
   !! Variance of a 1-D array, returned as real(dp).
   !!
   !! By default the unbiased sample variance (dividing by `n - 1`) is
   !! returned; pass `sample = .false.` for the population variance (dividing
   !! by `n`). Returns `0` when there are too few elements to define it.
   !! Supported input types are: integer(int32), integer(int64), real(sp),
   !! real(dp).
   !! Usage: `v = variance(array [, sample])`
      module procedure :: variance_int32
      module procedure :: variance_int64
      module procedure :: variance_sp
      module procedure :: variance_dp
   end interface variance

   interface std_dev
   !! Standard deviation of a 1-D array, returned as real(dp).
   !!
   !! Equal to `sqrt(variance(array, sample))`; see `variance` for the meaning
   !! of the optional `sample` flag.
   !! Usage: `s = std_dev(array [, sample])`
      module procedure :: std_dev_int32
      module procedure :: std_dev_int64
      module procedure :: std_dev_sp
      module procedure :: std_dev_dp
   end interface std_dev

   interface min_max
   !! Minimum and maximum of a 1-D array in a single length-2 result.
   !!
   !! Returns `[minval(array), maxval(array)]`, preserving the input type.
   !! The result is undefined for an empty array. Supported types are:
   !! integer(int32), integer(int64), real(sp), real(dp).
   !! Usage: `mm = min_max(array)  ! mm(1) = min, mm(2) = max`
      module procedure :: min_max_int32
      module procedure :: min_max_int64
      module procedure :: min_max_sp
      module procedure :: min_max_dp
   end interface min_max

contains

   pure function mean_int32(array) result(m)
      !! Mean of an integer(int32) array.
      integer(int32), intent(in) :: array(:)
      real(dp) :: m
      integer(default_int) :: n

      n = size(array, kind=default_int)
      if (n == 0) then
         m = 0.0_dp
      else
         m = sum(real(array, dp))/real(n, dp)
      end if
   end function mean_int32

   pure function mean_int64(array) result(m)
      !! Mean of an integer(int64) array.
      integer(int64), intent(in) :: array(:)
      real(dp) :: m
      integer(default_int) :: n

      n = size(array, kind=default_int)
      if (n == 0) then
         m = 0.0_dp
      else
         m = sum(real(array, dp))/real(n, dp)
      end if
   end function mean_int64

   pure function mean_sp(array) result(m)
      !! Mean of a real(sp) array.
      real(sp), intent(in) :: array(:)
      real(dp) :: m
      integer(default_int) :: n

      n = size(array, kind=default_int)
      if (n == 0) then
         m = 0.0_dp
      else
         m = sum(real(array, dp))/real(n, dp)
      end if
   end function mean_sp

   pure function mean_dp(array) result(m)
      !! Mean of a real(dp) array.
      real(dp), intent(in) :: array(:)
      real(dp) :: m
      integer(default_int) :: n

      n = size(array, kind=default_int)
      if (n == 0) then
         m = 0.0_dp
      else
         m = sum(array)/real(n, dp)
      end if
   end function mean_dp

   pure function variance_dp(array, sample) result(v)
      !! Variance of a real(dp) array (core implementation, two-pass).
      real(dp), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: v
      real(dp) :: m
      integer(default_int) :: n, denom
      logical :: use_sample

      use_sample = .true.
      if (present(sample)) use_sample = sample

      n = size(array, kind=default_int)
      if (use_sample) then
         denom = n - 1
      else
         denom = n
      end if

      if (denom <= 0) then
         v = 0.0_dp
      else
         m = sum(array)/real(n, dp)
         v = sum((array - m)**2)/real(denom, dp)
      end if
   end function variance_dp

   pure function variance_int32(array, sample) result(v)
      !! Variance of an integer(int32) array.
      integer(int32), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: v
      v = variance_dp(real(array, dp), sample)
   end function variance_int32

   pure function variance_int64(array, sample) result(v)
      !! Variance of an integer(int64) array.
      integer(int64), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: v
      v = variance_dp(real(array, dp), sample)
   end function variance_int64

   pure function variance_sp(array, sample) result(v)
      !! Variance of a real(sp) array.
      real(sp), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: v
      v = variance_dp(real(array, dp), sample)
   end function variance_sp

   pure function std_dev_dp(array, sample) result(s)
      !! Standard deviation of a real(dp) array.
      real(dp), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: s
      s = sqrt(variance_dp(array, sample))
   end function std_dev_dp

   pure function std_dev_int32(array, sample) result(s)
      !! Standard deviation of an integer(int32) array.
      integer(int32), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: s
      s = sqrt(variance_dp(real(array, dp), sample))
   end function std_dev_int32

   pure function std_dev_int64(array, sample) result(s)
      !! Standard deviation of an integer(int64) array.
      integer(int64), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: s
      s = sqrt(variance_dp(real(array, dp), sample))
   end function std_dev_int64

   pure function std_dev_sp(array, sample) result(s)
      !! Standard deviation of a real(sp) array.
      real(sp), intent(in) :: array(:)
      logical, intent(in), optional :: sample
      real(dp) :: s
      s = sqrt(variance_dp(real(array, dp), sample))
   end function std_dev_sp

   pure function min_max_int32(array) result(mm)
      !! Minimum and maximum of an integer(int32) array.
      integer(int32), intent(in) :: array(:)
      integer(int32) :: mm(2)
      mm(1) = minval(array)
      mm(2) = maxval(array)
   end function min_max_int32

   pure function min_max_int64(array) result(mm)
      !! Minimum and maximum of an integer(int64) array.
      integer(int64), intent(in) :: array(:)
      integer(int64) :: mm(2)
      mm(1) = minval(array)
      mm(2) = maxval(array)
   end function min_max_int64

   pure function min_max_sp(array) result(mm)
      !! Minimum and maximum of a real(sp) array.
      real(sp), intent(in) :: array(:)
      real(sp) :: mm(2)
      mm(1) = minval(array)
      mm(2) = maxval(array)
   end function min_max_sp

   pure function min_max_dp(array) result(mm)
      !! Minimum and maximum of a real(dp) array.
      real(dp), intent(in) :: array(:)
      real(dp) :: mm(2)
      mm(1) = minval(array)
      mm(2) = maxval(array)
   end function min_max_dp

end module pic_statistics
