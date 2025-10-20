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
   use pic_types, only: sp, dp, default_int
   implicit none
   private

   ! these are the cool overloaded interfaces, the pic_xyz function
   ! has the procedures pic_(type)xyz which will call the correct BLAS routine
   ! depending on the data type of the arguments
   ! this _needs_ allocatable arrays since we deduce shapes from the arrays themselves
   public :: pic_gemm, pic_gemv, pic_asum, pic_axpy, pic_copy, pic_dot, pic_scal, pic_iamax

   ! tested
   interface pic_gemm
      !! general interface of the BLAS GEMM routines, will call SGEMM, DGEMM, CGEMM
      !!
      !! Usage: call pic_gemm(A, B, C, [optional] transa, [optional] transb, [optional] alpha, [optional] beta)
      !!
      !! where A, B, C are matrices, transa and transb are optional transpose options,
      !! alpha and beta are optional scaling factors
      !!
      !! By default, if not specified transA and transB are "N" (no transpose),
      !! and alpha and beta are 1.0 and 0.0 respectively.
      !!
      !! The matrices A, B, C must be allocatable arrays, we deduce the shapes from them.
      module procedure :: pic_sgemm
      module procedure :: pic_dgemm
   end interface pic_gemm

   interface pic_gemv
      !! general interface of the BLAS GEMV routines, will call SGEMV, DGEMV, CGEMV, ZGEMV
      !!
      !! Usage: call pic_gemv(A, x, y, [optional] transa, [optional] alpha, [optional] beta)
      !!
      !! where A is a matrix, x and y are vectors, transa is an optional transpose option,
      !! alpha and beta are optional scaling factors.
      !!
      !! The matrix A must be an allocatable array, we deduce the shapes from it.
      !! TransA is "N" (no transpose) by default. And alpha and beta are 1.0 and 0.0 respectively.
      !!
      module procedure :: pic_sgemv
      module procedure :: pic_dgemv
   end interface pic_gemv
   ! tested
   interface pic_asum
      !! general interface of the BLAS ASUM routines, will call SASUM, DASUM, SCASUM, DZASUM
      !!
      !! Usage: result = pic_asum(x, incx)
      !!
      !! where x is a vector and incx is the increment, this will return the sum of the absolute values
      !! of the elements of x.
      !!
      !! The vector x must be an allocatable array, we deduce the shape from it.
      !! The increment incx is 1 by default.
      !!
      module procedure :: pic_sasum
      module procedure :: pic_dasum
      module procedure :: pic_scasum
      module procedure :: pic_dzasum
   end interface pic_asum

   interface pic_axpy
      !! general interface of the BLAS AXPY routines, will call SAXPY, DAXPY, CAXPY, ZAXPY
      !!
      !! Usage: call pic_axpy(n, alpha, x, incx, y, incy)
      !!
      !! where n is the number of elements, alpha is the scaling factor,
      !! x is the input vector, incx is the increment for x, y is the output vector,
      !! and incy is the increment for y.
      !!
      !! The vectors x and y must be allocatable arrays, we deduce the shapes from them.
      !! The increments incx and incy are 1 by default.
      !!
      module procedure :: pic_saxpy
      module procedure :: pic_daxpy
   end interface pic_axpy

   interface pic_copy
      !! general interface of the BLAS COPY routines, will call SCOPY, DCOPY, CCOPY, ZCOPY
      !!
      !! Usage: call pic_copy(x, y)
      !!
      !! where x is the input vector, y is the output vector.
      !! The vectors x and y must be allocatable arrays, we deduce the shapes from them.
      !!
      module procedure :: pic_scopy
      module procedure :: pic_dcopy
   end interface pic_copy

   interface pic_dot
      !! general interface of the BLAS DOT routines, will call SDOT, DDOT, CDOTC, ZDOTC
      !!
      !! Usage: result = pic_dot(x, y)
      !!
      !! where x is the input vector, y is the output vector.
      !! The vectors x and y must be allocatable arrays, we deduce the shapes from them.
      !!
      module procedure :: pic_sdot
      module procedure :: pic_ddot
      module procedure :: pic_cdotc
      module procedure :: pic_zdotc
   end interface pic_dot

   interface pic_scal
      !! general interface of the BLAS SCAL routines, will call SSCAL, DSCAL, CSCAL, ZSCAL
      !!
      !! Usage: call pic_scal(x, [optional] alpha)
      !!
      !! where x is the input vector, alpha is the scaling factor.
      !! The vector x must be an allocatable array, we deduce the shape from it.
      !! The scaling factor alpha is 1.0 by default.
      !!
      module procedure :: pic_sscal
      module procedure :: pic_dscal
   end interface pic_scal

   interface pic_iamax
      !! general interface of the BLAS IAMAX routines, will call ISAMAX, IDAMAX, ICAMAX, IZAMAX
      !!
      !! Usage: idx = pic_iamax(x, incx)
      !!
      !! where x is the input vector, incx is the increment.
      !! The vector x must be an allocatable array, we deduce the shape from it.
      !! The increment incx is 1 by default.
      !!
      module procedure :: pic_isamax
      module procedure :: pic_idamax
   end interface pic_iamax

   interface blas_asum
      !! this is the interface for the BLAS ASUM routines, it will call SASUM, DASUM, SCASUM, DZASUM
      !! Usage: result = blas_asum(x, incx)
      !! where x is the input vector, incx is the increment.
      !!
      !! This is not a public interface, it is used internally by pic_asum
      pure function sasum(n, x, incx) result(res_sasum)
         import :: sp, default_int
         implicit none
         real(sp) :: res_sasum
         real(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function sasum
      pure function dasum(n, x, incx) result(res_dasum)
         import :: dp, default_int
         implicit none
         real(dp) :: res_dasum
         real(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function dasum
      pure function scasum(n, x, incx) result(res_scasum)
         import :: sp, default_int
         implicit none
         real(sp) :: res_scasum
         complex(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function scasum
      pure function dzasum(n, x, incx) result(res_dzasum)
         import :: dp, default_int
         implicit none
         real(dp) :: res_dzasum
         complex(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function dzasum
   end interface blas_asum

   interface blas_axpy
      !! explicit interface for BLAS AXPY routines
      !!
      !! Usage: call blas_axpy(n, alpha, x, incx, y, incy)
      !!
      !! This is not a public interface, it is used internally by pic_axpy
      pure subroutine saxpy(n, alpha, x, incx, y, incy)
         import :: sp, default_int
         implicit none
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine saxpy
      pure subroutine daxpy(n, alpha, x, incx, y, incy)
         import :: dp, default_int
         implicit none
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine daxpy
      pure subroutine caxpy(n, alpha, x, incx, y, incy)
         import :: sp, default_int
         implicit none
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine caxpy
      pure subroutine zaxpy(n, alpha, x, incx, y, incy)
         import :: dp, default_int
         implicit none
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine zaxpy
   end interface blas_axpy

   interface blas_copy
      !! explicit interface for BLAS COPY routines
      !!
      !! Usage: call blas_copy(x, y)
      !!
      !! This is not a public interface, it is used internally by pic_copy
      pure subroutine scopy(n, x, incx, y, incy)
         import :: sp, default_int
         implicit none
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine scopy
      pure subroutine dcopy(n, x, incx, y, incy)
         import :: dp, default_int
         implicit none
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end subroutine dcopy
   end interface blas_copy

   interface blas_dot
      !! explicit interface for BLAS DOT routines
      !!
      !! Usage: result = blas_dot(x, y, incx, incy, n)
      !! This is not a public interface, it is used internally by pic_dot
      !!
      pure function sdot(n, x, incx, y, incy) result(res)
         import :: sp, default_int
         implicit none
         real(sp) :: res
         real(sp), intent(in) :: x(*)
         real(sp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function sdot
      pure function ddot(n, x, incx, y, incy) result(res)
         import :: dp, default_int
         implicit none
         real(dp) :: res
         real(dp), intent(in) :: x(*)
         real(dp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function ddot
      pure function cdotc(n, x, incx, y, incy) result(res)
         import :: sp, default_int
         implicit none
         complex(sp) :: res
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function cdotc
      pure function zdotc(n, x, incx, y, incy) result(res)
         import :: dp, default_int
         implicit none
         complex(dp) :: res
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(in) :: y(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         integer(default_int), intent(in) :: n
      end function zdotc
   end interface blas_dot

   interface blas_scal
      !! explicit interface for BLAS SCAL routines
      !!
      !! Usage: call blas_scal(n, alpha, x, incx)
      !!
      !! This is not a public interface, it is used internally by pic_scal
      pure subroutine sscal(n, alpha, x, incx)
         import :: sp, default_int
         implicit none
         real(sp), intent(in) :: alpha
         real(sp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine sscal
      pure subroutine dscal(n, alpha, x, incx)
         import :: dp, default_int
         implicit none
         real(dp), intent(in) :: alpha
         real(dp), intent(inout) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end subroutine dscal
   end interface blas_scal

   interface blas_iamax
      !! explicit interface for BLAS IAMAX routines
      !!
      !! Usage: idx = blas_iamax(x, incx)
      !!
      !! This is not a public interface, it is used internally by pic_iamax
      pure function isamax(n, x, incx) result(idx)
         import :: sp, default_int
         implicit none
         integer(default_int) :: idx
         real(sp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function isamax
      pure function idamax(n, x, incx) result(idx)
         import :: dp, default_int
         implicit none
         integer(default_int) :: idx
         real(dp), intent(in) :: x(*)
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: n
      end function idamax
   end interface blas_iamax

   interface blas_gemv
      !! explicit interface for BLAS GEMV routines
      !!
      !! Usage: call blas_gemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
      !!
      !! This is not a public interface, it is used internally by pic_gemv
      pure subroutine sgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: sp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         real(sp), intent(in) :: a(lda, *)
         real(sp), intent(in) :: x(*)
         real(sp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: beta
      end subroutine sgemv
      pure subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: dp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: x(*)
         real(dp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
      end subroutine dgemv
      pure subroutine cgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: sp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         complex(sp), intent(in) :: a(lda, *)
         complex(sp), intent(in) :: x(*)
         complex(sp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: beta
      end subroutine cgemv
      pure subroutine zgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
         import :: dp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: incx
         integer(default_int), intent(in) :: incy
         complex(dp), intent(in) :: a(lda, *)
         complex(dp), intent(in) :: x(*)
         complex(dp), intent(inout) :: y(*)
         character(len=1), intent(in) :: trans
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: beta
      end subroutine zgemv
   end interface blas_gemv

   interface blas_gemm
      !! explicit interface for BLAS GEMM routines
      !!
      !! Usage: call blas_gemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
      !!
      !! This is not a public interface, it is used internally by pic_gemm
      pure subroutine sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: sp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
         real(sp), intent(in) :: a(lda, *)
         real(sp), intent(in) :: b(ldb, *)
         real(sp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         real(sp), intent(in) :: alpha
         real(sp), intent(in) :: beta
      end subroutine sgemm
      pure subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: dp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
         real(dp), intent(in) :: a(lda, *)
         real(dp), intent(in) :: b(ldb, *)
         real(dp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         real(dp), intent(in) :: alpha
         real(dp), intent(in) :: beta
      end subroutine dgemm
      pure subroutine cgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: sp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
         complex(sp), intent(in) :: a(lda, *)
         complex(sp), intent(in) :: b(ldb, *)
         complex(sp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         complex(sp), intent(in) :: alpha
         complex(sp), intent(in) :: beta
      end subroutine cgemm
      pure subroutine zgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, &
           & beta, c, ldc)
         import :: dp, default_int
         implicit none
         integer(default_int), intent(in) :: m
         integer(default_int), intent(in) :: n
         integer(default_int), intent(in) :: k
         integer(default_int), intent(in) :: lda
         integer(default_int), intent(in) :: ldb
         integer(default_int), intent(in) :: ldc
         complex(dp), intent(in) :: a(lda, *)
         complex(dp), intent(in) :: b(ldb, *)
         complex(dp), intent(inout) :: c(ldc, *)
         character(len=1), intent(in) :: transa
         character(len=1), intent(in) :: transb
         complex(dp), intent(in) :: alpha
         complex(dp), intent(in) :: beta
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
         OP_A = "N"
      end if
      if (present(transb)) then
         OP_B = transb
      else
         OP_B = "N"
      end if

      ! check for the dimensions now
      if ((OP_A == "N" .or. OP_A == "n")) then
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
         OP_A = "N"
      end if
      if (present(transb)) then
         OP_B = transb
      else
         OP_B = "N"
      end if

      ! check for the dimensions now
      if ((OP_A == "N" .or. OP_A == "n")) then
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

   pure subroutine pic_sgemv(A, x, y, trans_a, alpha, beta)
      !! interface for single precision matrix-vector multiplication
      real(sp), intent(in) :: A(:, :)
      real(sp), intent(in) :: x(:)
      real(sp), intent(inout) :: y(:)
      character(len=1), intent(in), optional :: trans_a
      real(sp), intent(in), optional :: alpha
      real(sp), intent(in), optional :: beta
      real(sp) :: l_alpha, l_beta
      character(len=1) :: l_trans_a
      integer(default_int) :: incx, incy, m, n, lda
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
         l_trans_a = "n"
      end if
      incx = 1
      incy = 1
      lda = max(1, size(A, 1))
      m = size(A, 1)
      n = size(A, 2)
      call blas_gemv(l_trans_a, m, n, l_alpha, A, lda, x, incx, l_beta, y, incy)
   end subroutine pic_sgemv

   pure subroutine pic_dgemv(A, x, y, trans_a, alpha, beta)
      !! interface for double precision matrix-vector multiplication
      real(dp), intent(in) :: A(:, :)
      real(dp), intent(in) :: x(:)
      real(dp), intent(inout) :: y(:)
      character(len=1), intent(in), optional :: trans_a
      real(dp), intent(in), optional :: alpha
      real(dp), intent(in), optional :: beta
      real(dp) :: l_alpha, l_beta
      character(len=1) :: l_trans_a
      integer(default_int) :: incx, incy, m, n, lda
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
         l_trans_a = "n"
      end if
      incx = 1
      incy = 1
      lda = max(1, size(A, 1))
      m = size(A, 1)
      n = size(A, 2)
      call blas_gemv(l_trans_a, m, n, l_alpha, A, lda, x, incx, l_beta, y, incy)
   end subroutine pic_dgemv

   function pic_sasum(x) result(res)
      !! interface for single precision absolute sum
      real(sp), intent(in) :: x(:)
      real(sp) :: res
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      res = blas_asum(n, x, incx)
   end function pic_sasum

   function pic_dasum(x) result(res)
      !! interface for double precision absolute sum
      real(dp), intent(in) :: x(:)
      real(dp) :: res
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      res = blas_asum(n, x, incx)
   end function pic_dasum

   function pic_scasum(x) result(res)
      !! interface for single precision complex absolute sum
      complex(sp), intent(in) :: x(:)
      real(sp) :: res
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      res = blas_asum(n, x, incx)
   end function pic_scasum

   function pic_dzasum(x) result(res)
      !! interface for double precision complex absolute sum
      complex(dp), intent(in) :: x(:)
      real(dp) :: res
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      res = blas_asum(n, x, incx)
   end function pic_dzasum

   subroutine pic_saxpy(x, y, alpha)
      !! interface for single precision AXPY
      real(sp), intent(in) :: x(:)
      real(sp), intent(inout) :: y(:)
      real(sp), intent(in), optional :: alpha
      real(sp) :: l_alpha
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      call blas_axpy(n, l_alpha, x, incx, y, incy)
   end subroutine pic_saxpy

   subroutine pic_daxpy(x, y, alpha)
      !! interface for double precision AXPY
      real(dp), intent(in) :: x(:)
      real(dp), intent(inout) :: y(:)
      real(dp), intent(in), optional :: alpha
      real(dp) :: l_alpha
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_dp
      end if
      call blas_axpy(n, l_alpha, x, incx, y, incy)
   end subroutine pic_daxpy

   subroutine pic_caxpy(x, y, alpha)
      !! interface for single precision complex AXPY
      complex(sp), intent(in) :: x(:)
      complex(sp), intent(inout) :: y(:)
      complex(sp), intent(in), optional :: alpha
      complex(sp) :: l_alpha
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      call blas_axpy(n, l_alpha, x, incx, y, incy)
   end subroutine pic_caxpy

   subroutine pic_zaxpy(x, y, alpha)
      !! interface for double precision complex AXPY
      complex(dp), intent(in) :: x(:)
      complex(dp), intent(inout) :: y(:)
      complex(dp), intent(in), optional :: alpha
      complex(dp) :: l_alpha
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_dp
      end if
      call blas_axpy(n, l_alpha, x, incx, y, incy)
   end subroutine pic_zaxpy

   subroutine pic_scopy(x, y)
      !! interface for single precision copy
      real(sp), intent(in) :: x(:)
      real(sp), intent(inout) :: y(:)
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      call blas_copy(n, x, incx, y, incy)
   end subroutine pic_scopy

   subroutine pic_dcopy(x, y)
      !! interface for double precision copy
      real(dp), intent(in) :: x(:)
      real(dp), intent(inout) :: y(:)
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      call blas_copy(n, x, incx, y, incy)
   end subroutine pic_dcopy

   function pic_sdot(x, y) result(res)
      !! interface for single precision dot product
      real(sp), intent(in) :: x(:)
      real(sp), intent(in) :: y(:)
      real(sp) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end function pic_sdot

   function pic_ddot(x, y) result(res)
      !! interface for double precision dot product
      real(dp), intent(in) :: x(:)
      real(dp), intent(in) :: y(:)
      real(dp) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end function pic_ddot

   function pic_cdotc(x, y) result(res)
      !! interface for single precision complex dot product
      complex(sp), intent(in) :: x(:)
      complex(sp), intent(in) :: y(:)
      complex(sp) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end function pic_cdotc

   function pic_zdotc(x, y) result(res)
      !! interface for double precision complex dot product
      complex(dp), intent(in) :: x(:)
      complex(dp), intent(in) :: y(:)
      complex(dp) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end function pic_zdotc

   subroutine pic_sscal(x, alpha)
      !! interface for single precision scaling
      real(sp), intent(inout) :: x(:)
      real(sp), intent(in), optional :: alpha
      real(sp) :: l_alpha
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      call blas_scal(n, l_alpha, x, incx)
   end subroutine pic_sscal

   subroutine pic_dscal(x, alpha)
      !! interface for double precision scaling
      real(dp), intent(inout) :: x(:)
      real(dp), intent(in), optional :: alpha
      real(dp) :: l_alpha
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_dp
      end if
      call blas_scal(n, l_alpha, x, incx)
   end subroutine pic_dscal

   function pic_isamax(x) result(idx)
      !! interface for single precision index of maximum absolute value
      real(sp), intent(in) :: x(:)
      integer(default_int) :: idx
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      idx = blas_iamax(n, x, incx)
   end function pic_isamax
   function pic_idamax(x) result(idx)
      !! interface for double precision index of maximum absolute value
      real(dp), intent(in) :: x(:)
      integer(default_int) :: idx
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      idx = blas_iamax(n, x, incx)
   end function pic_idamax
end module pic_blas_interfaces
