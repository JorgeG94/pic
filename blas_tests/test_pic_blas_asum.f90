module test_pic_blas_interfaces_asum
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_asum
   use pic_types, only: sp, dp, default_int

   implicit none
   private
   public :: collect_pic_asum_tests
   integer(default_int), parameter :: size_of_asum = 5

contains

   subroutine collect_pic_asum_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_sasum", test_sasum), &
                  new_unittest("test_dasum", test_dasum), &
                  new_unittest("test_scasum", test_scasum), &
                  new_unittest("test_dcasum", test_dcasum) &
                  ]

   end subroutine collect_pic_asum_tests

   subroutine test_sasum(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(size_of_asum)
      real(sp), parameter :: tol = 1.0e-6_sp
      real(sp) :: result, expected
      expected = 15.0_sp
      A = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
      result = pic_asum(A)
      call check(error, (abs(expected - result) < tol))
      if (allocated(error)) return
   end subroutine test_sasum

   subroutine test_dasum(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(size_of_asum)
      real(dp), parameter :: tol = 1.0e-6_dp
      real(dp) :: result, expected
      expected = 15.0_dp
      A = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
      result = pic_asum(A)
      call check(error, (abs(expected - result) < tol))
      if (allocated(error)) return
   end subroutine test_dasum

   subroutine test_scasum(error)
      type(error_type), allocatable, intent(out) :: error
      complex(sp) :: A(size_of_asum)
      real(sp), parameter :: tol = 1.0e-6_sp
      complex(sp) :: result, expected
      expected = 15.0_sp
      A = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
      result = pic_asum(A)
      call check(error, (abs(expected - result) < tol))
      if (allocated(error)) return
   end subroutine test_scasum

   subroutine test_dcasum(error)
      type(error_type), allocatable, intent(out) :: error
      complex(dp) :: A(size_of_asum)
      real(dp), parameter :: tol = 1.0e-6_dp
      complex(dp) :: result, expected
      expected = 15.0_dp
      A = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
      result = pic_asum(A)
      call check(error, (abs(expected - result) < tol))
      if (allocated(error)) return
   end subroutine test_dcasum

end module test_pic_blas_interfaces_asum
