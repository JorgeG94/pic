module test_pic_blas_interfaces_scal
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_scal
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_scal_tests

contains

   subroutine collect_pic_scal_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 4
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_sscal_alpha", test_sscal_alpha)
      testsuite(2) = new_unittest("test_dscal_alpha", test_dscal_alpha)
      testsuite(3) = new_unittest("test_sscal_default", test_sscal_default)
      testsuite(4) = new_unittest("test_dscal_default", test_dscal_default)
   end subroutine collect_pic_scal_tests

   subroutine test_sscal_alpha(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), alpha, expected(3)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vector and scalar
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      alpha = 2.0_sp
      expected = alpha*x  ! Expected result after scaling

      call pic_scal(x, alpha)

      call check(error, all(abs(x - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sscal_alpha

   subroutine test_dscal_alpha(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), alpha, expected(3)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vector and scalar
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      alpha = 2.0_dp
      expected = alpha*x  ! Expected result after scaling

      call pic_scal(x, alpha)

      call check(error, all(abs(x - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dscal_alpha

   subroutine test_sscal_default(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), alpha, expected(3)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vector and scalar
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      alpha = 1.0_sp
      expected = alpha*x  ! Expected result after scaling

      call pic_scal(x)

      call check(error, all(abs(x - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sscal_default

   subroutine test_dscal_default(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), alpha, expected(3)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vector and scalar
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      alpha = 1.0_dp
      expected = alpha*x  ! Expected result after scaling

      call pic_scal(x)

      call check(error, all(abs(x - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dscal_default
end module test_pic_blas_interfaces_scal
