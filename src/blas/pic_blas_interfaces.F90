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

   ! these are the generic interfaces to the BLAS library
   public :: blas_asum, blas_axpy, blas_copy, blas_dot, blas_scal, &
             blas_iamax, blas_gemv, blas_gemm

   ! these are the cool overloaded interfaces, the pic_xyz function
   ! has the procedures pic_(type)xyz which will call the correct BLAS routine
   ! depending on the data type of the arguments
   ! this _needs_ allocatable arrays since we deduce shapes from the arrays themselves
   public :: pic_gemm, pic_gemv, pic_asum, pic_axpy, pic_copy, pic_dot, pic_scal, pic_iamax

   ! tested
   interface pic_gemm
      !! general interface of the BLAS GEMM routines, will call SGEMM, DGEMM, or ZGEMM
      module procedure :: pic_sgemm
      module procedure :: pic_dgemm
      module procedure :: pic_zgemm
   end interface pic_gemm
   ! tested
   interface pic_gemv
      !! general interface of the BLAS GEMV routines, will call SGEMV, DGEMV
      module procedure :: pic_sgemv
      module procedure :: pic_dgemv
   end interface pic_gemv
   ! tested
   interface pic_asum
      !! general interface of the BLAS SASUM routines, will call xASUM
      module procedure :: pic_sasum
      module procedure :: pic_dasum
      module procedure :: pic_scasum
      module procedure :: pic_dzasum
   end interface pic_asum

   interface pic_axpy
      !! general interface of the BLAS AXPY routines, will call SAXPY, DAXPY, CAXPY, ZAXPY
      module procedure :: pic_saxpy
      module procedure :: pic_daxpy
      module procedure :: pic_caxpy
      module procedure :: pic_zaxpy
   end interface pic_axpy

   interface pic_copy
      !! general interface of the BLAS COPY routines, will call SCOPY, DCOPY, CCOPY, ZCOPY
      module procedure :: pic_scopy
      module procedure :: pic_dcopy
      module procedure :: pic_ccopy
      module procedure :: pic_zcopy
   end interface pic_copy

   interface pic_dot
      !! general interface of the BLAS DOT routines, will call SDOT, DDOT, CDOTC, ZDOTC
      module procedure :: pic_sdot
      module procedure :: pic_ddot
      module procedure :: pic_cdotc
      module procedure :: pic_zdotc
   end interface pic_dot

   interface pic_scal
      !! general interface of the BLAS SCAL routines, will call SSCAL, DSCAL, CSCAL, ZSCAL
      module procedure :: pic_sscal
      module procedure :: pic_dscal
      module procedure :: pic_cscal
      module procedure :: pic_zscal
   end interface pic_scal

   interface pic_iamax
      !! general interface of the BLAS IAMAX routines, will call ISAMAX, IDAMAX, ICAMAX, IZAMAX
      module procedure :: pic_isamax
      module procedure :: pic_idamax
      module procedure :: pic_icamax
      module procedure :: pic_izamax
   end interface pic_iamax

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
         l_trans_a = 'n'
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

   subroutine pic_ccopy(x, y)
      !! interface for single precision complex copy
      complex(sp), intent(in) :: x(:)
      complex(sp), intent(inout) :: y(:)
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      call blas_copy(n, x, incx, y, incy)
   end subroutine pic_ccopy

   subroutine pic_zcopy(x, y)
      !! interface for double precision complex copy
      complex(dp), intent(in) :: x(:)
      complex(dp), intent(inout) :: y(:)
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      call blas_copy(n, x, incx, y, incy)
   end subroutine pic_zcopy

   subroutine pic_sdot(x, y, res)
      !! interface for single precision dot product
      real(sp), intent(in) :: x(:)
      real(sp), intent(in) :: y(:)
      real(sp), intent(out) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end subroutine pic_sdot

   subroutine pic_ddot(x, y, res)
      !! interface for double precision dot product
      real(dp), intent(in) :: x(:)
      real(dp), intent(in) :: y(:)
      real(dp), intent(out) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end subroutine pic_ddot

   subroutine pic_cdotc(x, y, res)
      !! interface for single precision complex dot product
      complex(sp), intent(in) :: x(:)
      complex(sp), intent(in) :: y(:)
      complex(sp), intent(out) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end subroutine pic_cdotc

   subroutine pic_zdotc(x, y, res)
      !! interface for double precision complex dot product
      complex(dp), intent(in) :: x(:)
      complex(dp), intent(in) :: y(:)
      complex(dp), intent(out) :: res
      integer(default_int) :: n, incx, incy
      n = size(x)
      incx = 1
      incy = 1
      res = blas_dot(n, x, incx, y, incy)
   end subroutine pic_zdotc

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

   subroutine pic_cscal(x, alpha)
      !! interface for single precision complex scaling
      complex(sp), intent(inout) :: x(:)
      complex(sp), intent(in), optional :: alpha
      complex(sp) :: l_alpha
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_sp
      end if
      call blas_scal(n, l_alpha, x, incx)
   end subroutine pic_cscal

   subroutine pic_zscal(x, alpha)
      !! interface for double precision complex scaling
      complex(dp), intent(inout) :: x(:)
      complex(dp), intent(in), optional :: alpha
      complex(dp) :: l_alpha
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      if (present(alpha)) then
         l_alpha = alpha
      else
         l_alpha = 1.0_dp
      end if
      call blas_scal(n, l_alpha, x, incx)
   end subroutine pic_zscal

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
   function pic_icamax(x) result(idx)
      !! interface for single precision complex index of maximum absolute value
      complex(sp), intent(in) :: x(:)
      integer(default_int) :: idx
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      idx = blas_iamax(n, x, incx)
   end function pic_icamax
   function pic_izamax(x) result(idx)
      !! interface for double precision complex index of maximum absolute value
      complex(dp), intent(in) :: x(:)
      integer(default_int) :: idx
      integer(default_int) :: n, incx
      n = size(x)
      incx = 1
      idx = blas_iamax(n, x, incx)
   end function pic_izamax

end module pic_blas_interfaces
