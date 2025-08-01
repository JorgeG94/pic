module test_pic_blas_interfaces_sgemv
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_gemv
   use pic_types, only: sp, dp, default_int
   implicit none
   private
   public :: collect_pic_sgemv_tests

contains

   subroutine collect_pic_sgemv_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer(default_int), parameter :: ntests = 5
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_sgemv_basic", test_sgemv_basic)
      testsuite(2) = new_unittest("test_sgemv_transpose", test_sgemv_transpose)
      testsuite(3) = new_unittest("test_sgemv_alpha_beta", test_sgemv_alpha_beta)
      testsuite(4) = new_unittest("test_sgemv_identity", test_sgemv_identity)
      testsuite(5) = new_unittest("test_sgemv_beta_accumulate", test_sgemv_beta_accumulate)
   end subroutine collect_pic_sgemv_tests

   subroutine test_sgemv_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), x(2), y(2), expected(2)
      real(sp), parameter :: tol = 1.0e-6_sp

      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      x = [5.0_sp, 6.0_sp]
      y = [0.0_sp, 0.0_sp]

      call pic_gemv(A, x, y)

      expected = [1.0_sp*5.0_sp + 3.0_sp*6.0_sp, 2.0_sp*5.0_sp + 4.0_sp*6.0_sp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemv_basic

   subroutine test_sgemv_transpose(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), x(2), y(2), expected(2)
      real(sp), parameter :: tol = 1.0e-6_sp

      A = reshape([1.0_sp, 3.0_sp, 2.0_sp, 4.0_sp], [2, 2])  ! A^T = [1,2;3,4]
      x = [5.0_sp, 6.0_sp]
      y = [0.0_sp, 0.0_sp]

      call pic_gemv(A, x, y, trans_a="T")

      expected = [1.0_sp*5.0_sp + 3.0_sp*6.0_sp, 2.0_sp*5.0_sp + 4.0_sp*6.0_sp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemv_transpose

   subroutine test_sgemv_alpha_beta(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), x(2), y(2), expected(2)
      real(sp), parameter :: tol = 1.0e-6_sp
      real(sp), parameter :: alpha = 2.0_sp, beta = 3.0_sp

      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      x = [1.0_sp, 1.0_sp]
      y = [1.0_sp, 1.0_sp]

      call pic_gemv(A, x, y, alpha=alpha, beta=beta)

      expected = [11.0_sp, 15.0_sp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemv_alpha_beta

   subroutine test_sgemv_identity(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(3, 3), x(3), y(3), expected(3)
      real(sp), parameter :: tol = 1.0e-6_sp

      A = 0.0_sp
      A(1, 1) = 1.0_sp
      A(2, 2) = 1.0_sp
      A(3, 3) = 1.0_sp
      x = [7.0_sp, 8.0_sp, 9.0_sp]
      y = [0.0_sp, 0.0_sp, 0.0_sp]

      call pic_gemv(A, x, y)

      expected = x
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemv_identity

   subroutine test_sgemv_beta_accumulate(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), x(2), y(2), expected(2)
      real(sp), parameter :: tol = 1.0e-6_sp

      A = reshape([1.0_sp, 0.0_sp, 0.0_sp, 1.0_sp], [2, 2])  ! Identity
      x = [10.0_sp, 20.0_sp]
      y = [1.0_sp, 2.0_sp]

      expected = x + y  ! since A*x = x and beta = 1, y = x + y
      call pic_gemv(A, x, y, beta=1.0_sp)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemv_beta_accumulate

end module test_pic_blas_interfaces_sgemv
