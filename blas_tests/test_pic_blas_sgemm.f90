module test_pic_blas_interfaces_sgemm
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_blas_interfaces, only: pic_gemm
   use pic_types, only: sp, dp, default_int  ! assuming sp is your single precision kind
   implicit none
   private
   public :: collect_pic_sgemm_tests

contains

   subroutine collect_pic_sgemm_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_sgemm_basic", test_sgemm_basic), &
                  new_unittest("test_sgemm_transpose_a", test_sgemm_transpose_a), &
                  new_unittest("test_sgemm_transpose_b", test_sgemm_transpose_b), &
                  new_unittest("test_sgemm_transpose_both", test_sgemm_transpose_both), &
                  new_unittest("test_sgemm_alpha_beta", test_sgemm_alpha_beta), &
                  new_unittest("test_sgemm_identity", test_sgemm_identity), &
                  new_unittest("test_sgemm_rectangular", test_sgemm_rectangular), &
                  new_unittest("test_sgemm_beta_accumulate", test_sgemm_beta_accumulate) &
                  ]

   end subroutine collect_pic_sgemm_tests

   subroutine test_sgemm_basic(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Simple 2x2 matrix multiplication: C = A * B
      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      B = reshape([5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2])
      C = 0.0_sp

      call pic_gemm(A, B, C)

      ! Expected result: [19, 22; 43, 50]
      expected = reshape([23.0_sp, 34.0_sp, 31.0_sp, 46.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_basic

   subroutine test_sgemm_transpose_a(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! C = A^T * B
      A = reshape([1.0_sp, 3.0_sp, 2.0_sp, 4.0_sp], [2, 2])  ! A^T will be [1,2;3,4]
      B = reshape([5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2])
      C = 0.0_sp

      call pic_gemm(A, B, C, transa="T")

      ! Expected result: A^T * B = [1,2;3,4] * [5,6;7,8] = [19,22;43,50]
      expected = reshape([23.0_sp, 34.0_sp, 31.0_sp, 46.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_transpose_a

   subroutine test_sgemm_transpose_b(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! C = A * B^T
      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      B = reshape([5.0_sp, 7.0_sp, 6.0_sp, 8.0_sp], [2, 2])  ! B^T will be [5,6;7,8]
      C = 0.0_sp

      call pic_gemm(A, B, C, transb="T")

      ! Expected result: A * B^T = [1,3;2,4] * [5,6;7,8] = [26,30;38,44]
      expected = reshape([23.0_sp, 34.0_sp, 31.0_sp, 46.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_transpose_b

   subroutine test_sgemm_transpose_both(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! C = A^T * B^T
      A = reshape([1.0_sp, 3.0_sp, 2.0_sp, 4.0_sp], [2, 2])  ! A^T = [1,2;3,4]
      B = reshape([5.0_sp, 7.0_sp, 6.0_sp, 8.0_sp], [2, 2])  ! B^T = [5,6;7,8]
      C = 0.0_sp

      call pic_gemm(A, B, C, transa="T", transb="T")

      ! Expected result: A^T * B^T = [1,2;3,4] * [5,6;7,8] = [19,22;43,50]
      expected = reshape([23.0_sp, 34.0_sp, 31.0_sp, 46.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_transpose_both

   subroutine test_sgemm_alpha_beta(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp
      real(sp), parameter :: alpha = 2.0_sp, beta = 3.0_sp

      ! C = alpha * A * B + beta * C
      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      B = reshape([1.0_sp, 0.0_sp, 0.0_sp, 1.0_sp], [2, 2])  ! Identity matrix
      C = reshape([1.0_sp, 1.0_sp, 1.0_sp, 1.0_sp], [2, 2])  ! Initial values

      call pic_gemm(A, B, C, alpha=alpha, beta=beta)

      ! Expected: 2*A*I + 3*C_initial = 2*A + 3*ones = [2,6;4,8] + [3,3;3,3] = [5,9;7,11]
      expected = reshape([5.0_sp, 7.0_sp, 9.0_sp, 11.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_alpha_beta

   subroutine test_sgemm_identity(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(3, 3), B(3, 3), C(3, 3)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Test multiplication by identity matrix
      A = reshape([1.0_sp, 4.0_sp, 7.0_sp, 2.0_sp, 5.0_sp, 8.0_sp, 3.0_sp, 6.0_sp, 9.0_sp], [3, 3])
      B = reshape([1.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, 1.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, 1.0_sp], [3, 3])  ! Identity
      C = 0.0_sp

      call pic_gemm(A, B, C)

      ! A * I should equal A
      call check(error, all(abs(C - A) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_identity

   subroutine test_sgemm_rectangular(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 3), B(3, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Test rectangular matrices: (2x3) * (3x2) = (2x2)
      A = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp], [2, 3])
      B = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp], [3, 2])
      C = 0.0_sp

      call pic_gemm(A, B, C)

      ! Expected result: [22, 28; 49, 64]
      expected = reshape([22.0_sp, 28.0_sp, 49.0_sp, 64.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_rectangular

   subroutine test_sgemm_beta_accumulate(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: A(2, 2), B(2, 2), C(2, 2), expected(2, 2)
      real(sp), parameter :: tol = 1.0e-6_sp

      ! Test beta accumulation: C = A*B + C (beta=1)
      A = reshape([1.0_sp, 0.0_sp, 0.0_sp, 1.0_sp], [2, 2])  ! Identity
      B = reshape([2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp], [2, 2])
      C = reshape([10.0_sp, 20.0_sp, 30.0_sp, 40.0_sp], [2, 2])  ! Initial values

      call pic_gemm(A, B, C, beta=1.0_sp)

      ! Expected: I*B + C = B + C = [2,4;3,5] + [10,30;20,40] = [12,34;23,45]
      expected = reshape([12.0_sp, 23.0_sp, 34.0_sp, 45.0_sp], [2, 2])

      call check(error, all(abs(C - expected) < tol))
      if (allocated(error)) return
   end subroutine test_sgemm_beta_accumulate

end module test_pic_blas_interfaces_sgemm
