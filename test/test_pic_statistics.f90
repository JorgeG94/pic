! SPDX-License-Identifier: MIT
module test_pic_statistics
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: int32, int64, sp, dp
   use pic_statistics, only: mean, variance, std_dev, min_max
   implicit none
   private
   public :: collect_pic_statistics_tests

contains

   subroutine collect_pic_statistics_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("mean_int32", test_mean_int32), &
                  new_unittest("mean_int64", test_mean_int64), &
                  new_unittest("mean_sp", test_mean_sp), &
                  new_unittest("mean_dp", test_mean_dp), &
                  new_unittest("mean_empty", test_mean_empty), &
                  new_unittest("variance_sample", test_variance_sample), &
                  new_unittest("variance_population", test_variance_population), &
                  new_unittest("variance_too_few", test_variance_too_few), &
                  new_unittest("variance_int32", test_variance_int32), &
                  new_unittest("variance_int64", test_variance_int64), &
                  new_unittest("variance_sp", test_variance_sp), &
                  new_unittest("std_dev_dp", test_std_dev_dp), &
                  new_unittest("std_dev_sp", test_std_dev_sp), &
                  new_unittest("std_dev_int32", test_std_dev_int32), &
                  new_unittest("std_dev_int64", test_std_dev_int64), &
                  new_unittest("min_max_int32", test_min_max_int32), &
                  new_unittest("min_max_int64", test_min_max_int64), &
                  new_unittest("min_max_sp", test_min_max_sp), &
                  new_unittest("min_max_dp", test_min_max_dp), &
                  new_unittest("min_max_single", test_min_max_single) &
                  ]
   end subroutine collect_pic_statistics_tests

   subroutine test_mean_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: v(4)
      v = [1_int32, 2_int32, 3_int32, 4_int32]
      call check(error, mean(v), 2.5_dp, "mean of 1..4", thr=1.0e-12_dp)
   end subroutine test_mean_int32

   subroutine test_mean_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: v(4)
      v = [1_int64, 2_int64, 3_int64, 4_int64]
      call check(error, mean(v), 2.5_dp, "mean int64 of 1..4", thr=1.0e-12_dp)
   end subroutine test_mean_int64

   subroutine test_mean_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: v(3)
      v = [2.0_sp, 4.0_sp, 6.0_sp]
      call check(error, mean(v), 4.0_dp, "mean sp of 2,4,6", thr=1.0e-6_dp)
   end subroutine test_mean_sp

   subroutine test_mean_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(3)
      v = [2.0_dp, 4.0_dp, 6.0_dp]
      call check(error, mean(v), 4.0_dp, "mean of 2,4,6", thr=1.0e-12_dp)
   end subroutine test_mean_dp

   subroutine test_mean_empty(error)
      !! Mean of an empty array is defined here as 0, for every input type.
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vdp(:)
      real(sp), allocatable :: vsp(:)
      integer(int32), allocatable :: vi32(:)
      integer(int64), allocatable :: vi64(:)

      allocate (vdp(0), vsp(0), vi32(0), vi64(0))
      call check(error, mean(vdp), 0.0_dp, "empty dp is zero", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, mean(vsp), 0.0_dp, "empty sp is zero", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, mean(vi32), 0.0_dp, "empty int32 is zero", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, mean(vi64), 0.0_dp, "empty int64 is zero", thr=1.0e-12_dp)
   end subroutine test_mean_empty

   subroutine test_variance_sample(error)
      !! Sample variance of 1..5: mean 3, sum sq dev 10, /(n-1)=/4 = 2.5.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(5)
      v = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
      call check(error, variance(v), 2.5_dp, "sample variance", thr=1.0e-12_dp)
   end subroutine test_variance_sample

   subroutine test_variance_population(error)
      !! Population variance of 1..5: sum sq dev 10, /n=/5 = 2.0.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(5)
      v = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
      call check(error, variance(v, sample=.false.), 2.0_dp, "population variance", thr=1.0e-12_dp)
   end subroutine test_variance_population

   subroutine test_variance_too_few(error)
      !! Sample variance of a single element has no degrees of freedom -> 0.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(1)
      v = [3.0_dp]
      call check(error, variance(v), 0.0_dp, "single element sample variance is zero", thr=1.0e-12_dp)
   end subroutine test_variance_too_few

   subroutine test_variance_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: v(5)
      v = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]
      call check(error, variance(v), 2.5_dp, "int32 sample variance", thr=1.0e-12_dp)
   end subroutine test_variance_int32

   subroutine test_variance_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: v(5)
      v = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64]
      call check(error, variance(v, sample=.false.), 2.0_dp, "int64 population variance", thr=1.0e-12_dp)
   end subroutine test_variance_int64

   subroutine test_variance_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: v(5)
      v = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
      call check(error, variance(v), 2.5_dp, "sp sample variance", thr=1.0e-6_dp)
   end subroutine test_variance_sp

   subroutine test_std_dev_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(5)
      v = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
      call check(error, std_dev(v), sqrt(2.5_dp), "sample std dev", thr=1.0e-12_dp)
   end subroutine test_std_dev_dp

   subroutine test_std_dev_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: v(5)
      v = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
      call check(error, std_dev(v, sample=.false.), sqrt(2.0_dp), "population std dev from sp", thr=1.0e-6_dp)
   end subroutine test_std_dev_sp

   subroutine test_std_dev_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: v(5)
      v = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]
      call check(error, std_dev(v), sqrt(2.5_dp), "int32 sample std dev", thr=1.0e-12_dp)
   end subroutine test_std_dev_int32

   subroutine test_std_dev_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: v(5)
      v = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64]
      call check(error, std_dev(v, sample=.false.), sqrt(2.0_dp), "int64 population std dev", thr=1.0e-12_dp)
   end subroutine test_std_dev_int64

   subroutine test_min_max_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: v(5), mm(2)
      v = [3_int32, 1_int32, 4_int32, 1_int32, 5_int32]
      mm = min_max(v)
      call check(error, mm(1), 1_int32, "min")
      if (allocated(error)) return
      call check(error, mm(2), 5_int32, "max")
   end subroutine test_min_max_int32

   subroutine test_min_max_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: v(5), mm(2)
      v = [3_int64, 1_int64, 4_int64, 1_int64, 5_int64]
      mm = min_max(v)
      call check(error, mm(1), 1_int64, "min")
      if (allocated(error)) return
      call check(error, mm(2), 5_int64, "max")
   end subroutine test_min_max_int64

   subroutine test_min_max_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: v(4), mm(2)
      v = [2.5_sp, -1.0_sp, 3.5_sp, 0.0_sp]
      mm = min_max(v)
      call check(error, mm(1), -1.0_sp, "min", thr=1.0e-6_sp)
      if (allocated(error)) return
      call check(error, mm(2), 3.5_sp, "max", thr=1.0e-6_sp)
   end subroutine test_min_max_sp

   subroutine test_min_max_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(4), mm(2)
      v = [2.5_dp, -1.0_dp, 3.5_dp, 0.0_dp]
      mm = min_max(v)
      call check(error, mm(1), -1.0_dp, "min", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, mm(2), 3.5_dp, "max", thr=1.0e-12_dp)
   end subroutine test_min_max_dp

   subroutine test_min_max_single(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: v(1), mm(2)
      v = [7.0_dp]
      mm = min_max(v)
      call check(error, mm(1), 7.0_dp, "single min", thr=1.0e-12_dp)
      if (allocated(error)) return
      call check(error, mm(2), 7.0_dp, "single max", thr=1.0e-12_dp)
   end subroutine test_min_max_single

end module test_pic_statistics
