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

   interface pic_gemm
      module procedure :: pic_sgemm
      module procedure :: pic_dgemm
      module procedure :: pic_zgemm
   end interface pic_gemm

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

contains

   pure subroutine pic_sgemm(A, B, C, transa, transb, alpha, beta)
      !! interface for single precision matrix multiplication
      real(sp), intent(in) :: A(:, :)
      real(sp), intent(in) :: B(:, :)
      real(sp), intent(inout) :: C(:, :)
      character(len=1), intent(in), optional :: transa
      character(len=1), intent(in), optional :: transb
      real(sp), intent(in), optional :: alpha
      real(sp), intent(in), optional :: beta
      character(len=1) :: OP_A, OP_B
      real(sp) :: l_alpha, l_beta
      integer(default_int) :: m, n, k, lda, ldb, ldc

      ! first check for the constants
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      if (present(beta)) then
         l_beta = beta
      else
         l_beta = 0.0_sp
      end if
      ! check the OP options, maybe this should not be optional
      if (present(transa)) then
         OP_A = transa
      else
         OP_A = 'N'
      end if
      if (present(transb)) then
         OP_B = transb
      else
         OP_B = 'N'
      end if

      ! check for the dimensions now
      if ((OP_A == 'N' .or. OP_A == 'n')) then
         k = size(A, 2)
      else
         k = size(A, 1)
      end if

      ! get LDA, LDB, and LDC
      lda = max(1, size(A, 1))
      ldb = max(1, size(B, 1))
      ldc = max(1, size(C, 1))
      m = size(C, 1)
      n = size(C, 2)

      call blas_gemm(OP_A, OP_B, m, n, k, l_alpha, A, lda, B, ldb, l_beta, C, ldc)

   end subroutine pic_sgemm

   pure subroutine pic_dgemm(A, B, C, transa, transb, alpha, beta)
      !! interface for single precision matrix multiplication
      real(dp), intent(in) :: A(:, :)
      real(dp), intent(in) :: B(:, :)
      real(dp), intent(inout) :: C(:, :)
      character(len=1), intent(in), optional :: transa
      character(len=1), intent(in), optional :: transb
      real(dp), intent(in), optional :: alpha
      real(dp), intent(in), optional :: beta
      character(len=1) :: OP_A, OP_B
      real(dp) :: l_alpha, l_beta
      integer(default_int) :: m, n, k, lda, ldb, ldc

      ! first check for the constants
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      if (present(beta)) then
         l_beta = beta
      else
         l_beta = 0.0_sp
      end if
      ! check the OP options, maybe this should not be optional
      if (present(transa)) then
         OP_A = transa
      else
         OP_A = 'N'
      end if
      if (present(transb)) then
         OP_B = transb
      else
         OP_B = 'N'
      end if

      ! check for the dimensions now
      if ((OP_A == 'N' .or. OP_A == 'n')) then
         k = size(A, 2)
      else
         k = size(A, 1)
      end if

      ! get LDA, LDB, and LDC
      lda = max(1, size(A, 1))
      ldb = max(1, size(B, 1))
      ldc = max(1, size(C, 1))
      m = size(C, 1)
      n = size(C, 2)

      call blas_gemm(OP_A, OP_B, m, n, k, l_alpha, A, lda, B, ldb, l_beta, C, ldc)

   end subroutine pic_dgemm
   pure subroutine pic_zgemm(A, B, C, transa, transb, alpha, beta)
      !! interface for single precision matrix multiplication
      complex(dp), intent(in) :: A(:, :)
      complex(dp), intent(in) :: B(:, :)
      complex(dp), intent(inout) :: C(:, :)
      character(len=1), intent(in), optional :: transa
      character(len=1), intent(in), optional :: transb
      complex(dp), intent(in), optional :: alpha
      complex(dp), intent(in), optional :: beta
      character(len=1) :: OP_A, OP_B
      complex(dp) :: l_alpha, l_beta
      integer(default_int) :: m, n, k, lda, ldb, ldc

      ! first check for the constants
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      if (present(beta)) then
         l_beta = beta
      else
         l_beta = 0.0_sp
      end if
      ! check the OP options, maybe this should not be optional
      if (present(transa)) then
         OP_A = transa
      else
         OP_A = 'N'
      end if
      if (present(transb)) then
         OP_B = transb
      else
         OP_B = 'N'
      end if

      ! check for the dimensions now
      if ((OP_A == 'N' .or. OP_A == 'n')) then
         k = size(A, 2)
      else
         k = size(A, 1)
      end if

      ! get LDA, LDB, and LDC
      lda = max(1, size(A, 1))
      ldb = max(1, size(B, 1))
      ldc = max(1, size(C, 1))
      m = size(C, 1)
      n = size(C, 2)

      call blas_gemm(OP_A, OP_B, m, n, k, l_alpha, A, lda, B, ldb, l_beta, C, ldc)

   end subroutine pic_zgemm

   pure subroutine pic_sgemv(A, x, y, trans_a, alpha, beta)
      real(sp), intent(in) :: A(:, :)
      real(sp), intent(in) :: x(:)
      real(sp), intent(inout) :: y(:)
      character(len=1), intent(in), optional :: trans_a
      real(sp), intent(in), optional :: alpha
      real(sp), intent(in), optional :: beta
      real(sp) :: l_alpha, l_beta
      character(len=1) :: l_trans_a
      integer :: incx, incy, m, n, lda
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      if (present(beta)) then
         l_beta = beta
      else
         l_beta = 0.0_sp
      end if
      if (present(trans_a)) then
         l_trans_a = trans_a
      else
         l_trans_a = 'n'
      end if
      incx = 1
      incy = 1
      lda = max(1, size(A, 1))
      m = size(A, 1)
      n = size(A, 2)
      call blas_gemv(l_trans_a, m, n, l_alpha, A, lda, x, incx, l_beta, y, incy)
   end subroutine pic_sgemv
   pure subroutine pic_dgemv(A, x, y, trans_a, alpha, beta)
      real(dp), intent(in) :: A(:, :)
      real(dp), intent(in) :: x(:)
      real(dp), intent(inout) :: y(:)
      character(len=1), intent(in), optional :: trans_a
      real(dp), intent(in), optional :: alpha
      real(dp), intent(in), optional :: beta
      real(dp) :: l_alpha, l_beta
      character(len=1) :: l_trans_a
      integer :: incx, incy, m, n, lda
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      if (present(beta)) then
         l_beta = beta
      else
         l_beta = 0.0_sp
      end if
      if (present(trans_a)) then
         l_trans_a = trans_a
      else
         l_trans_a = 'n'
      end if
      incx = 1
      incy = 1
      lda = max(1, size(A, 1))
      m = size(A, 1)
      n = size(A, 2)
      call blas_gemv(l_trans_a, m, n, l_alpha, A, lda, x, incx, l_beta, y, incy)
   end subroutine pic_dgemv

end module pic_blas_interfaces
