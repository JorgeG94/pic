! SPDX-License-Identifier: MIT
module test_pic_math
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: int32, int64, sp, dp
   use pic_math, only: clamp, is_in_range, is_close
   implicit none
   private
   public :: collect_pic_math_tests

contains

   subroutine collect_pic_math_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("clamp_int32", test_clamp_int32), &
                  new_unittest("clamp_int64", test_clamp_int64), &
                  new_unittest("clamp_sp", test_clamp_sp), &
                  new_unittest("clamp_dp", test_clamp_dp), &
                  new_unittest("clamp_array", test_clamp_array), &
                  new_unittest("is_in_range_int32", test_is_in_range_int32), &
                  new_unittest("is_in_range_int64", test_is_in_range_int64), &
                  new_unittest("is_in_range_sp", test_is_in_range_sp), &
                  new_unittest("is_in_range_dp", test_is_in_range_dp), &
                  new_unittest("is_close_dp_equal", test_is_close_dp_equal), &
                  new_unittest("is_close_dp_far", test_is_close_dp_far), &
                  new_unittest("is_close_dp_tol", test_is_close_dp_tol), &
                  new_unittest("is_close_dp_atol_zero", test_is_close_dp_atol_zero), &
                  new_unittest("is_close_sp", test_is_close_sp) &
                  ]
   end subroutine collect_pic_math_tests

   subroutine test_clamp_int32(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, clamp(5_int32, 0_int32, 10_int32), 5_int32, "within")
      if (allocated(error)) return
      call check(error, clamp(-3_int32, 0_int32, 10_int32), 0_int32, "below")
      if (allocated(error)) return
      call check(error, clamp(42_int32, 0_int32, 10_int32), 10_int32, "above")
   end subroutine test_clamp_int32

   subroutine test_clamp_int64(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, clamp(5_int64, 0_int64, 10_int64), 5_int64, "within")
      if (allocated(error)) return
      call check(error, clamp(-3_int64, 0_int64, 10_int64), 0_int64, "below")
      if (allocated(error)) return
      call check(error, clamp(42_int64, 0_int64, 10_int64), 10_int64, "above")
   end subroutine test_clamp_int64

   subroutine test_clamp_sp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, clamp(0.5_sp, 0.0_sp, 1.0_sp), 0.5_sp, "within", thr=1.0e-6_sp)
      if (allocated(error)) return
      call check(error, clamp(-1.0_sp, 0.0_sp, 1.0_sp), 0.0_sp, "below", thr=1.0e-6_sp)
      if (allocated(error)) return
      call check(error, clamp(2.0_sp, 0.0_sp, 1.0_sp), 1.0_sp, "above", thr=1.0e-6_sp)
   end subroutine test_clamp_sp

   subroutine test_clamp_dp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, clamp(0.5_dp, 0.0_dp, 1.0_dp), 0.5_dp, "within", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, clamp(-1.0_dp, 0.0_dp, 1.0_dp), 0.0_dp, "below", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, clamp(2.0_dp, 0.0_dp, 1.0_dp), 1.0_dp, "above", thr=1.0e-12_dp)
   end subroutine test_clamp_dp

   subroutine test_clamp_array(error)
      !! clamp is elemental, so it applies over whole arrays.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: v(5), c(5)
      v = [-5_int32, 0_int32, 5_int32, 10_int32, 15_int32]
      c = clamp(v, 0_int32, 10_int32)
      call check(error, all(c == [0_int32, 0_int32, 5_int32, 10_int32, 10_int32]), "elemental clamp")
   end subroutine test_clamp_array

   subroutine test_is_in_range_int32(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_in_range(5_int32, 0_int32, 10_int32), "within")
      if (allocated(error)) return
      call check(error, is_in_range(0_int32, 0_int32, 10_int32), "low boundary inclusive")
      if (allocated(error)) return
      call check(error, is_in_range(10_int32, 0_int32, 10_int32), "high boundary inclusive")
      if (allocated(error)) return
      call check(error,.not. is_in_range(11_int32, 0_int32, 10_int32), "above is out")
   end subroutine test_is_in_range_int32

   subroutine test_is_in_range_int64(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_in_range(5_int64, 0_int64, 10_int64), "within")
      if (allocated(error)) return
      call check(error,.not. is_in_range(-1_int64, 0_int64, 10_int64), "below is out")
   end subroutine test_is_in_range_int64

   subroutine test_is_in_range_sp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_in_range(0.5_sp, 0.0_sp, 1.0_sp), "within")
      if (allocated(error)) return
      call check(error,.not. is_in_range(1.5_sp, 0.0_sp, 1.0_sp), "above is out")
   end subroutine test_is_in_range_sp

   subroutine test_is_in_range_dp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_in_range(0.5_dp, 0.0_dp, 1.0_dp), "within")
      if (allocated(error)) return
      call check(error,.not. is_in_range(1.5_dp, 0.0_dp, 1.0_dp), "above is out")
   end subroutine test_is_in_range_dp

   subroutine test_is_close_dp_equal(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_close(1.0_dp, 1.0_dp), "identical values are close")
   end subroutine test_is_close_dp_equal

   subroutine test_is_close_dp_far(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error,.not. is_close(1.0_dp, 2.0_dp), "1 and 2 are not close")
   end subroutine test_is_close_dp_far

   subroutine test_is_close_dp_tol(error)
      !! Values differing by less than a supplied rtol are close; a tighter
      !! rtol rejects them.
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_close(1.0_dp, 1.0_dp + 1.0e-9_dp, rtol=1.0e-6_dp), "within rtol")
      if (allocated(error)) return
      call check(error,.not. is_close(1.0_dp, 1.1_dp, rtol=1.0e-6_dp), "outside rtol")
   end subroutine test_is_close_dp_tol

   subroutine test_is_close_dp_atol_zero(error)
      !! Relative tolerance alone cannot bridge a comparison against exact
      !! zero; an absolute tolerance can.
      type(error_type), allocatable, intent(out) :: error
      call check(error,.not. is_close(1.0e-12_dp, 0.0_dp), "tiny vs zero not close by rtol")
      if (allocated(error)) return
      call check(error, is_close(1.0e-12_dp, 0.0_dp, atol=1.0e-9_dp), "close once atol given")
   end subroutine test_is_close_dp_atol_zero

   subroutine test_is_close_sp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_close(1.0_sp, 1.0_sp), "identical sp are close")
      if (allocated(error)) return
      call check(error,.not. is_close(1.0_sp, 2.0_sp), "1 and 2 sp not close")
   end subroutine test_is_close_sp

end module test_pic_math
