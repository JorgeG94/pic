module test_pic_blas_interfaces_axpy
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_axpy_tests

contains

   subroutine collect_pic_axpy_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 4
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_saxpy_basic", test_saxpy_basic)
      testsuite(2) = new_unittest("test_daxpy_basic", test_daxpy_basic)
      testsuite(3) = new_unittest("test_saxpy_alpha", test_saxpy_alpha)
      testsuite(4) = new_unittest("test_daxpy_alpha", test_daxpy_alpha)
   end subroutine collect_pic_axpy_tests

   subroutine test_saxpy_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), y(3), expected(3)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Initialize vectors
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      y = [4.0_sp, 5.0_sp, 6.0_sp]
      expected = [5.0_sp, 7.0_sp, 9.0_sp]  ! Expected result after saxpy

      call pic_axpy(x, y)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_saxpy_basic

   subroutine test_daxpy_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), y(3), expected(3)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Initialize vectors
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      y = [4.0_dp, 5.0_dp, 6.0_dp]
      expected = [5.0_dp, 7.0_dp, 9.0_dp]  ! Expected result after daxpy

      call pic_axpy(x, y)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_daxpy_basic

   subroutine test_saxpy_alpha(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: x(3), y(3), expected(3)
      real(sp), parameter :: alpha = 2.0_sp, tol = 1.0e-6_sp

      ! Initialize vectors
      x = [1.0_sp, 2.0_sp, 3.0_sp]
      y = [4.0_sp, 5.0_sp, 6.0_sp]
      expected = [6.0_sp, 9.0_sp, 12.0_sp]  ! Expected result after saxpy with alpha

      call pic_axpy(x, y, alpha)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_saxpy_alpha

   subroutine test_daxpy_alpha(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x(3), y(3), expected(3)
      real(dp), parameter :: alpha = 2.0_dp, tol = 1.0e-6_dp

      ! Initialize vectors
      x = [1.0_dp, 2.0_dp, 3.0_dp]
      y = [4.0_dp, 5.0_dp, 6.0_dp]
      expected = [6.0_dp, 9.0_dp, 12.0_dp]  ! Expected result after daxpy with alpha

      call pic_axpy(x, y, alpha)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_daxpy_alpha

end module test_pic_blas_interfaces_axpy
