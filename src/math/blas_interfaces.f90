module pic_blas_interfaces
   use pic_types
   implicit none

   interface gemm
      pure subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
         import :: dp, default_int
         integer(kind=default_int), intent(in) :: lda
         integer(kind=default_int), intent(in) :: ldb
         integer(kind=default_int), intent(in) :: ldc
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: b(ldb, *)
         real(dp), intent(inout) :: c(ldc, *)
         character, intent(in) :: transa
         character, intent(in) :: transb
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
         integer(kind=default_int), intent(in) :: m
         integer(kind=default_int), intent(in) :: n
         integer(kind=default_int), intent(in) :: k
      end subroutine
   end interface gemm

   interface symm
      pure subroutine dsymm(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc)
         import :: dp, default_int
         integer(kind=default_int), intent(in) :: lda
         integer(kind=default_int), intent(in) :: ldb
         integer(kind=default_int), intent(in) :: ldc
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: b(ldb, *)
         real(dp), intent(inout) :: c(ldc, *)
         character, intent(in) :: side
         character, intent(in) :: uplo
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
         integer(kind=default_int), intent(in) :: m
         integer(kind=default_int), intent(in) :: n
      end subroutine dsymm
   end interface symm

   interface spmv
      pure subroutine dspmv(uplo, n, alpha, a, x, incx, beta, y, incy)
         import :: dp, default_int
         integer(kind=default_int), intent(in) :: n
         integer(kind=default_int), intent(in) :: incx
         integer(kind=default_int), intent(in) :: incy
         real(dp), intent(in) :: a(*)
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         character, intent(in) :: uplo
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
      end subroutine dspmv
   end interface spmv

   interface gemv
      pure subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: dp, default_int
         integer(kind=default_int), intent(in) :: lda
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         character, intent(in) :: trans
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
         integer(kind=default_int), intent(in) :: m
         integer(kind=default_int), intent(in) :: n
         integer(kind=default_int), intent(in) :: incx
         integer(kind=default_int), intent(in) :: incy
      end subroutine dgemv
   end interface gemv

end module pic_blas_interfaces
