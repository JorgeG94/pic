module test_pic_blas_iamax
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_blas_iamax_tests

contains

   subroutine collect_pic_blas_iamax_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_siamax", test_siamax)
      testsuite(2) = new_unittest("test_diamax", test_diamax)
   end subroutine collect_pic_blas_iamax_tests

   subroutine test_siamax(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3)
      integer :: result, expected
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vector
      x = [1.0_sp, -2.0_sp, 3.0_sp]
      expected = 3  ! Expected index of maximum absolute value

      result = pic_iamax(x)

      call check(error, abs(result - expected) < tol)
      if (allocated(error)) return
   end subroutine test_siamax

   subroutine test_diamax(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3)
      integer :: result, expected
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vector
      x = [1.0_dp, -2.0_dp, 3.0_dp]
      expected = 3  ! Expected index of maximum absolute value

      result = pic_iamax(x)

      call check(error, abs(result - expected) < tol)
      if (allocated(error)) return
   end subroutine test_diamax

end module test_pic_blas_iamax
