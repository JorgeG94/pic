module test_pic_blas_interfaces_dgemm
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces
   use pic_types, only: sp, dp, default_int  ! assuming sp is your single precision kind
   implicit none
   private
   public :: collect_pic_dgemm_tests

contains

   subroutine collect_pic_dgemm_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 8
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_dgemm_basic", test_dgemm_basic)
      testsuite(2) = new_unittest("test_dgemm_transpose_a", test_dgemm_transpose_a)
      testsuite(3) = new_unittest("test_dgemm_transpose_b", test_dgemm_transpose_b)
      testsuite(4) = new_unittest("test_dgemm_transpose_both", test_dgemm_transpose_both)
      testsuite(5) = new_unittest("test_dgemm_alpha_beta", test_dgemm_alpha_beta)
      testsuite(6) = new_unittest("test_dgemm_identity", test_dgemm_identity)
      testsuite(7) = new_unittest("test_dgemm_rectangular", test_dgemm_rectangular)
      testsuite(8) = new_unittest("test_dgemm_beta_accumulate", test_dgemm_beta_accumulate)
   end subroutine collect_pic_dgemm_tests

   subroutine test_dgemm_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Simple 2x2 matrix multiplication: C = A * B
      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      B = reshape([5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2])
      C = 0.0_dp

      call pic_gemm(A, B, C)

      ! Expected result: [19, 22; 43, 50]
      expected = reshape([23.0_dp, 34.0_dp, 31.0_dp, 46.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_basic

   subroutine test_dgemm_transpose_a(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! C = A^T * B
      A = reshape([1.0_dp, 3.0_dp, 2.0_dp, 4.0_dp], [2, 2])  ! A^T will be [1,2;3,4]
      B = reshape([5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2])
      C = 0.0_dp

      call pic_gemm(A, B, C, transa='T')

      ! Expected result: A^T * B = [1,2;3,4] * [5,6;7,8] = [19,22;43,50]
      expected = reshape([23.0_dp, 34.0_dp, 31.0_dp, 46.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_transpose_a

   subroutine test_dgemm_transpose_b(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! C = A * B^T
      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      B = reshape([5.0_dp, 7.0_dp, 6.0_dp, 8.0_dp], [2, 2])  ! B^T will be [5,6;7,8]
      C = 0.0_dp

      call pic_gemm(A, B, C, transb='T')

      ! Expected result: A * B^T = [1,3;2,4] * [5,6;7,8] = [26,30;38,44]
      expected = reshape([23.0_dp, 34.0_dp, 31.0_dp, 46.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_transpose_b

   subroutine test_dgemm_transpose_both(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! C = A^T * B^T
      A = reshape([1.0_dp, 3.0_dp, 2.0_dp, 4.0_dp], [2, 2])  ! A^T = [1,2;3,4]
      B = reshape([5.0_dp, 7.0_dp, 6.0_dp, 8.0_dp], [2, 2])  ! B^T = [5,6;7,8]
      C = 0.0_dp

      call pic_gemm(A, B, C, transa='T', transb='T')

      ! Expected result: A^T * B^T = [1,2;3,4] * [5,6;7,8] = [19,22;43,50]
      expected = reshape([23.0_dp, 34.0_dp, 31.0_dp, 46.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_transpose_both

   subroutine test_dgemm_alpha_beta(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp
      real(dp), parameter :: alpha = 2.0_dp, beta = 3.0_dp

      ! C = alpha * A * B + beta * C
      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      B = reshape([1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [2, 2])  ! Identity matrix
      C = reshape([1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp], [2, 2])  ! Initial values

      call pic_gemm(A, B, C, alpha=alpha, beta=beta)

      ! Expected: 2*A*I + 3*C_initial = 2*A + 3*ones = [2,6;4,8] + [3,3;3,3] = [5,9;7,11]
      expected = reshape([5.0_dp, 7.0_dp, 9.0_dp, 11.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_alpha_beta

   subroutine test_dgemm_identity(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(3, 3), B(3, 3), C(3, 3)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Test multiplication by identity matrix
      A = reshape([1.0_dp, 4.0_dp, 7.0_dp, 2.0_dp, 5.0_dp, 8.0_dp, 3.0_dp, 6.0_dp, 9.0_dp], [3, 3])
      B = reshape([1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [3, 3])  ! Identity
      C = 0.0_dp

      call pic_gemm(A, B, C)

      ! A * I should equal A
      call check(error, all(abs(C - A) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_identity

   subroutine test_dgemm_rectangular(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 3), B(3, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Test rectangular matrices: (2x3) * (3x2) = (2x2)
      A = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [2, 3])
      B = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [3, 2])
      C = 0.0_dp

      call pic_gemm(A, B, C)

      ! Expected result: [22, 28; 49, 64]
      expected = reshape([22.0_dp, 28.0_dp, 49.0_dp, 64.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_rectangular

   subroutine test_dgemm_beta_accumulate(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(dp), parameter :: tol = 1.0e-6_dp

      ! Test beta accumulation: C = A*B + C (beta=1)
      A = reshape([1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [2, 2])  ! Identity
      B = reshape([2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], [2, 2])
      C = reshape([10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp], [2, 2])  ! Initial values

      call pic_gemm(A, B, C, beta=1.0_dp)

      ! Expected: I*B + C = B + C = [2,4;3,5] + [10,30;20,40] = [12,34;23,45]
      expected = reshape([12.0_dp, 23.0_dp, 34.0_dp, 45.0_dp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_dgemm_beta_accumulate

end module test_pic_blas_interfaces_dgemm
