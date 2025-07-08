module test_pic_blas_interfaces_copy
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_copy
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_copy_tests

contains

   subroutine collect_pic_copy_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_scopy", test_scopy)
      testsuite(2) = new_unittest("test_dcopy", test_dcopy)
   end subroutine collect_pic_copy_tests

   subroutine test_scopy(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), y(3), expected(3)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vectors
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      y = 0.0_sp
      expected = x  ! Expected result after copy

      call pic_copy(x, y)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_scopy

   subroutine test_dcopy(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), y(3), expected(3)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vectors
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      y = 0.0_dp
      expected = x  ! Expected result after copy

      call pic_copy(x, y)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dcopy
end module test_pic_blas_interfaces_copy
