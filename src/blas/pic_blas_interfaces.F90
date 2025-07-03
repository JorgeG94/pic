!! this file contains the interfaces for the BLAS routines of all levels
!! I might consider splitting them up later but alas, I don't have the time now
!! the idea of this file is to provide something akin to
!! interface blas_gemm
!!  subroutine sgemm()
!!  subroutine dgemm()
!!   ... etc,
!! end interface blas_gemm
!! so that I can use the same interface for all BLAS routines

module pic_blas_interfaces
  !! pic_blas_interfaces.F90 provides the interfaces for the BLAS routines
  !! the idea is to have a two level interface, first pic_blas_xyz which
  !! is the way programmers will use BLAS, it'll do some checks and then
  !! call the "overloaded" BLAS interfaces to call the correct BLAS routine
   use pic_types
   implicit none

   interface blas_asum
      pure function sasum(n, x, incx)
         import :: sp, default_int
         real(sp) :: sasum
         real(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function sasum
      pure function dasum(n, x, incx)
         import :: dp, default_int
         real(dp) :: dasum
         real(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function dasum
      pure function scasum(n, x, incx)
         import :: sp, default_int
         real(sp) :: scasum
         complex(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function scasum
      pure function dzasum(n, x, incx)
         import :: dp, default_int
         real(dp) :: dzasum
         complex(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function dzasum
   end interface blas_asum

   interface blas_axpy
      pure subroutine saxpy(n, alpha, x, incx, y, incy)
         import :: sp, default_int
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine saxpy
      pure subroutine daxpy(n, alpha, x, incx, y, incy)
         import :: dp, default_int
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine daxpy
      pure subroutine caxpy(n, alpha, x, incx, y, incy)
         import :: sp, default_int
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine caxpy
      pure subroutine zaxpy(n, alpha, x, incx, y, incy)
         import :: dp, default_int
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine zaxpy
   end interface blas_axpy

   interface blas_copy
      pure subroutine scopy(n, x, incx, y, incy)
         import :: sp, default_int
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine scopy
      pure subroutine dcopy(n, x, incx, y, incy)
         import :: dp, default_int
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine dcopy
      pure subroutine ccopy(n, x, incx, y, incy)
         import :: sp, default_int
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine ccopy
      pure subroutine zcopy(n, x, incx, y, incy)
         import :: dp, default_int
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine zcopy
   end interface blas_copy

   interface blas_dot
      pure function sdot(n, x, incx, y, incy) result(res)
         import :: sp, default_int
         real(sp) :: res
         real(sp), intent(in) :: x(*)
         real(sp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function sdot
      pure function ddot(n, x, incx, y, incy) result(res)
         import :: dp, default_int
         real(dp) :: res
         real(dp), intent(in) :: x(*)
         real(dp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function ddot
      pure function cdotc(n, x, incx, y, incy) result(res)
         import :: sp, default_int
         complex(sp) :: res
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function cdotc
      pure function zdotc(n, x, incx, y, incy) result(res)
         import :: dp, default_int
         complex(dp) :: res
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function zdotc
   end interface blas_dot

   interface blas_scal
      pure subroutine sscal(n, alpha, x, incx)
         import :: sp, default_int
         real(sp), intent(in) :: alpha
         real(sp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine sscal
      pure subroutine dscal(n, alpha, x, incx)
         import :: dp, default_int
         real(dp), intent(in) :: alpha
         real(dp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine dscal
      pure subroutine cscal(n, alpha, x, incx)
         import :: sp, default_int
         complex(sp), intent(in) :: alpha
         complex(sp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine cscal
      pure subroutine zscal(n, alpha, x, incx)
         import :: dp, default_int
         complex(dp), intent(in) :: alpha
         complex(dp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine zscal
   end interface blas_scal

   interface blas_iamax
      pure function isamax(n, x, incx) result(idx)
         import :: sp, default_int
         integer(default_int) :: idx
         real(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function isamax
      pure function idamax(n, x, incx) result(idx)
         import :: dp, default_int
         integer(default_int) :: idx
         real(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function idamax
      pure function icamax(n, x, incx) result(idx)
         import :: sp, default_int
         integer(default_int) :: idx
         complex(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function icamax
      pure function izamax(n, x, incx) result(idx)
         import :: dp, default_int
         integer(default_int) :: idx
         complex(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function izamax
   end interface blas_iamax

   interface blas_gemv
      pure subroutine sgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: sp, default_int
         real(sp), intent(in) :: a(lda, *)
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
      end subroutine sgemv
      pure subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: dp, default_int
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
      end subroutine dgemv
      pure subroutine cgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: sp, default_int
         complex(sp), intent(in) :: a(lda, *)
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
      end subroutine cgemv
      pure subroutine zgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: dp, default_int
         complex(dp), intent(in) :: a(lda, *)
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
      end subroutine zgemv
   end interface blas_gemv

   interface blas_gemm
      pure subroutine sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: sp, default_int
         real(sp), intent(in) :: a(lda, *)
         real(sp), intent(in) :: b(ldb, *)
         real(sp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
      end subroutine sgemm
      pure subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: dp, default_int
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: b(ldb, *)
         real(dp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
      end subroutine dgemm
      pure subroutine cgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: sp, default_int
         complex(sp), intent(in) :: a(lda, *)
         complex(sp), intent(in) :: b(ldb, *)
         complex(sp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
      end subroutine cgemm
      pure subroutine zgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: dp, default_int
         complex(dp), intent(in) :: a(lda, *)
         complex(dp), intent(in) :: b(ldb, *)
         complex(dp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: beta
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
      end subroutine zgemm
   end interface blas_gemm

end module pic_blas_interfaces
