module test_pic_blas_interfaces_dot
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_dot
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_blas_dot_tests

contains

   subroutine collect_pic_blas_dot_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_sdot", test_sdot)
      testsuite(2) = new_unittest("test_ddot", test_ddot)
   end subroutine collect_pic_blas_dot_tests

   subroutine test_sdot(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), y(3)
      real(sp) :: result, expected
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vectors
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      y = [4.0_sp, 5.0_sp, 6.0_sp]
      expected = dot_product(x, y)  ! Expected result: 32.0_sp

      result = pic_dot(x, y)

      call check(error, abs(result - expected) < tol)
      if (allocated(error)) return
   end subroutine test_sdot

   subroutine test_ddot(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), y(3)
      real(dp) :: result, expected
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vectors
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      y = [4.0_dp, 5.0_dp, 6.0_dp]
      expected = dot_product(x, y)  ! Expected result: 32.0_dp

      result = pic_dot(x, y)

      call check(error, abs(result - expected) < tol)
      if (allocated(error)) return
   end subroutine test_ddot

end module test_pic_blas_interfaces_dot
