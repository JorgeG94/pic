module test_pic_blas_interfaces_dgemv
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_gemv
   use pic_types, only: dp, default_int
   implicit none
   private
   public :: collect_pic_dgemv_tests

contains

   subroutine collect_pic_dgemv_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 5
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_dgemv_basic", test_dgemv_basic)
      testsuite(2) = new_unittest("test_dgemv_transpose", test_dgemv_transpose)
      testsuite(3) = new_unittest("test_dgemv_alpha_beta", test_dgemv_alpha_beta)
      testsuite(4) = new_unittest("test_dgemv_identity", test_dgemv_identity)
      testsuite(5) = new_unittest("test_dgemv_beta_accumulate", test_dgemv_beta_accumulate)
   end subroutine collect_pic_dgemv_tests

   subroutine test_dgemv_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), x(2), y(2), expected(2)
      real(dp), parameter :: tol = 1.0e-6_dp

      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      x = [5.0_dp, 6.0_dp]
      y = [0.0_dp, 0.0_dp]

      call pic_gemv(A, x, y)

      expected = [1.0_dp*5.0_dp + 3.0_dp*6.0_dp, 2.0_dp*5.0_dp + 4.0_dp*6.0_dp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemv_basic

   subroutine test_dgemv_transpose(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), x(2), y(2), expected(2)
      real(dp), parameter :: tol = 1.0e-6_dp

      A = reshape([1.0_dp, 3.0_dp, 2.0_dp, 4.0_dp], [2, 2])  ! A^T = [1,2;3,4]
      x = [5.0_dp, 6.0_dp]
      y = [0.0_dp, 0.0_dp]

      call pic_gemv(A, x, y, trans_a='T')

      expected = [1.0_dp*5.0_dp + 3.0_dp*6.0_dp, 2.0_dp*5.0_dp + 4.0_dp*6.0_dp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemv_transpose

   subroutine test_dgemv_alpha_beta(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), x(2), y(2), expected(2)
      real(dp), parameter :: tol = 1.0e-6_dp
      real(dp), parameter :: alpha = 2.0_dp, beta = 3.0_dp

      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      x = [1.0_dp, 1.0_dp]
      y = [1.0_dp, 1.0_dp]

      call pic_gemv(A, x, y, alpha=alpha, beta=beta)

      expected = [11.0_dp, 15.0_dp]
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemv_alpha_beta

   subroutine test_dgemv_identity(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(3, 3), x(3), y(3), expected(3)
      real(dp), parameter :: tol = 1.0e-6_dp

      A = 0.0_dp
      A(1, 1) = 1.0_dp
      A(2, 2) = 1.0_dp
      A(3, 3) = 1.0_dp
      x = [7.0_dp, 8.0_dp, 9.0_dp]
      y = [0.0_dp, 0.0_dp, 0.0_dp]

      call pic_gemv(A, x, y)

      expected = x
      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemv_identity

   subroutine test_dgemv_beta_accumulate(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), x(2), y(2), expected(2)
      real(dp), parameter :: tol = 1.0e-6_dp

      A = reshape([1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [2, 2])  ! Identity
      x = [10.0_dp, 20.0_dp]
      y = [1.0_dp, 2.0_dp]

      expected = x + y  ! since A*x = x and beta = 1, y = x + y
      call pic_gemv(A, x, y, beta=1.0_dp)

      call check(error, all(abs(y - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemv_beta_accumulate

end module test_pic_blas_interfaces_dgemv
