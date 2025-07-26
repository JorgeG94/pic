!! pic array contains L0.5 BLAS level routines, as in things that could be use in
!! lieu of blas if you don't have it but if you do, please don't use these routines
module pic_array
  !! Provides interfaces to array operating routines, current support is only for
  !! 1 and 2d arrays with plans to cover up to 4d ones. I don't know why you'd use 5?
   use pic_types, only: sp, dp, int32, int64, default_int
   implicit none
   private

   public :: fill, copy, pic_transpose, pic_sum

   interface fill
  !! fill provides a generic interface to assing a value
  !! alpha of types (int32, int64, sp, dp) as defined in pic_types.F90
  !! The inteface supports filling 1d and 2d arrays of the specified
  !! variables
  !!
  !! Usage: call fill(array, value)
  !!
  !! This subroutine is threaded for performance purposes beyond a certian
  !! size of vector/matrices.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
  !!
      module procedure fill_vector_int32
      module procedure fill_vector_int64
      module procedure fill_vector_sp
      module procedure fill_vector_dp
      module procedure fill_matrix_int32
      module procedure fill_matrix_int64
      module procedure fill_matrix_sp
      module procedure fill_matrix_dp
   end interface

   interface copy
  !! copy provides a blas-less implementation of xcopy where x is (i,s,d) icopy, scopy, dcopy
  !! if you built pic with BLAS use pic_copy instead, I will not beat BLAS
  !! copy is implemented for (int32, int64, sp, dp) for 1 and 2d arrays of the same types
  !!
  !! Usage: call copy(destination, source)
  !!
  !! This subroutine is threaded for performance purposes beyond a certian
  !! size of vector/matrices.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
  !!
      module procedure copy_vector_int32
      module procedure copy_vector_int64
      module procedure copy_vector_sp
      module procedure copy_vector_dp
      module procedure copy_matrix_int32
      module procedure copy_matrix_int64
      module procedure copy_matrix_sp
      module procedure copy_matrix_dp
   end interface

   interface pic_transpose
  !! pic_transpose provides a blas-less, threaded alternative to the Fortran transpose intrinsic
  !! which will be slow for large matrix sizes. pic_transpose does not assume symmetric matrices
  !!
  !! pic_transpose is implemented for (int32, int64, sp, dp) 2d arrays
  !!
  !! Usage: call pic_transpose(matrix_to_transpose, result)
  !!
  !! This subroutine is threaded for performance purposes beyond a certian
  !! size of vector/matrices.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
  !!
      module procedure transpose_matrix_int32
      module procedure transpose_matrix_int64
      module procedure transpose_matrix_sp
      module procedure transpose_matrix_dp
   end interface

   interface pic_sum
  !! pic_sum provides a threaded alternative to the sum(array) Fortran intrinsic which will
  !! be too slow for large sizes of vectors and matrices. Note that this provides the total
  !! sum. As opposed to the blas alternative XASUM which does the absolute sum
  !!
  !! pic_sum is implemented for (int32, int64, sp, dp) 1 and 2d arrays
  !!
  !! Usage: result = pic_sum(array)
  !!
  !! This subroutine is threaded for performance purposes beyond a certian
  !! size of vector/matrices.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
  !!
      module procedure sum_vector_int32
      module procedure sum_vector_int64
      module procedure sum_vector_sp
      module procedure sum_vector_dp
      module procedure sum_matrix_int32
      module procedure sum_matrix_int64
      module procedure sum_matrix_sp
      module procedure sum_matrix_dp
   end interface pic_sum

   ! potentially implement a shallow copy? nah?
   integer(default_int), parameter :: block_size = 32
    !! This is the size to block over for matrices for performance purposes
   integer(default_int), parameter :: min_n_elements_parallel = 1024*1024
    !! If an array (vector or matrix) has more than these elements, it will
    !! use the openmp threaded parallelism otherwise it will use Fortran intrinsics
    !! this will only be a problem for really large arrays

contains

   subroutine fill_vector_int32(vector, alpha)
      integer(int32), intent(inout) :: vector(:)
      integer(int32), intent(in)    :: alpha
      integer(default_int) :: i, n_elements
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_int32

   subroutine fill_vector_int64(vector, alpha)
      integer(int64), intent(inout) :: vector(:)
      integer(int64), intent(in)    :: alpha
      integer(default_int) :: i, n_elements
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_int64

   subroutine fill_vector_sp(vector, alpha)
      real(sp), intent(inout) :: vector(:)
      real(sp), intent(in)    :: alpha
      integer(default_int) :: i, n_elements
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_sp

   subroutine fill_vector_dp(vector, alpha)
      real(dp), intent(inout) :: vector(:)
      real(dp), intent(in)    :: alpha
      integer(default_int) :: i, n_elements
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_dp

   subroutine fill_matrix_int32(matrix, alpha)
      integer(int32), intent(inout) :: matrix(:, :)
      integer(int32), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     matrix(i, j) = alpha
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         matrix = alpha
      end if
   end subroutine fill_matrix_int32

   subroutine fill_matrix_int64(matrix, alpha)
      integer(int64), intent(inout) :: matrix(:, :)
      integer(int64), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     matrix(i, j) = alpha
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         matrix = alpha
      end if
   end subroutine fill_matrix_int64

   subroutine fill_matrix_sp(matrix, alpha)
      real(sp), intent(inout) :: matrix(:, :)
      real(sp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     matrix(i, j) = alpha
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         matrix = alpha
      end if
   end subroutine fill_matrix_sp

   subroutine fill_matrix_dp(matrix, alpha)
      real(dp), intent(inout) :: matrix(:, :)
      real(dp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     matrix(i, j) = alpha
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         matrix = alpha
      end if
   end subroutine fill_matrix_dp

   subroutine copy_vector_int32(dest, source)
      integer(int32), intent(inout) :: dest(:)
      integer(int32), intent(in)    :: source(:)
      integer(default_int) :: i, n_elements
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      n_elements = size(dest, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp paralle do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_int32

   subroutine copy_vector_int64(dest, source)
      integer(int64), intent(inout) :: dest(:)
      integer(int64), intent(in)    :: source(:)
      integer(default_int) :: i, n_elements
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      n_elements = size(dest, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp paralle do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_int64

   subroutine copy_vector_sp(dest, source)
      real(sp), intent(inout) :: dest(:)
      real(sp), intent(in)    :: source(:)
      integer(default_int) :: i, n_elements
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      n_elements = size(dest, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp paralle do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_sp

   subroutine copy_vector_dp(dest, source)
      real(dp), intent(inout) :: dest(:)
      real(dp), intent(in)    :: source(:)
      integer(default_int) :: i, n_elements
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      n_elements = size(dest, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp paralle do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_dp

   subroutine copy_matrix_int32(dest, source)
      integer(int32), intent(inout) :: dest(:, :)
      integer(int32), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     dest(i, j) = source(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_matrix_int32

   subroutine copy_matrix_int64(dest, source)
      integer(int64), intent(inout) :: dest(:, :)
      integer(int64), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     dest(i, j) = source(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_matrix_int64

   subroutine copy_matrix_sp(dest, source)
      real(sp), intent(inout) :: dest(:, :)
      real(sp), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     dest(i, j) = source(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_matrix_sp

   subroutine copy_matrix_dp(dest, source)
      real(dp), intent(inout) :: dest(:, :)
      real(dp), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj, n_elements
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      n_elements = rows*cols
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     dest(i, j) = source(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_matrix_dp

   subroutine transpose_matrix_int32(A, B)
      integer(int32), intent(in)  :: A(:, :)
      integer(int32), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols
      integer(default_int) :: n_elements

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      n_elements = rows*cols

      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     B(j, i) = A(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         B = transpose(A)
      end if
   end subroutine transpose_matrix_int32

   subroutine transpose_matrix_int64(A, B)
      integer(int64), intent(in)  :: A(:, :)
      integer(int64), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols
      integer(default_int) :: n_elements

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      n_elements = rows*cols

      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     B(j, i) = A(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         B = transpose(A)
      end if
   end subroutine transpose_matrix_int64

   subroutine transpose_matrix_sp(A, B)
      real(sp), intent(in)  :: A(:, :)
      real(sp), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols
      integer(default_int) :: n_elements

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      n_elements = rows*cols

      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     B(j, i) = A(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         B = transpose(A)
      end if
   end subroutine transpose_matrix_sp

   subroutine transpose_matrix_dp(A, B)
      real(dp), intent(in)  :: A(:, :)
      real(dp), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols
      integer(default_int) :: n_elements

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      n_elements = rows*cols

      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     B(j, i) = A(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         B = transpose(A)
      end if
   end subroutine transpose_matrix_dp

   function sum_vector_int32(vector) result(res)
      integer(int32), intent(in)  :: vector(:)
      integer(int32) :: res
      integer(default_int) :: i, n_elements
      res = 0_int32
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, n_elements
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_int32
   function sum_vector_int64(vector) result(res)
      integer(int64), intent(in)  :: vector(:)
      integer(int64) :: res
      integer(default_int) :: i, n_elements
      res = 0_int64
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, n_elements
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_int64
   function sum_vector_sp(vector) result(res)
      real(sp), intent(in)  :: vector(:)
      real(sp) :: res
      integer(default_int) :: i, n_elements
      res = 0_sp
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, n_elements
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_sp
   function sum_vector_dp(vector) result(res)
      real(dp), intent(in)  :: vector(:)
      real(dp) :: res
      integer(default_int) :: i, n_elements
      res = 0_dp
      n_elements = size(vector, 1)
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, n_elements
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_dp

   function sum_matrix_int32(matrix) result(res)
      integer(int32), intent(in) :: matrix(:, :)
      integer(int32) :: res
      integer(default_int) :: cols, rows, n_elements, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      n_elements = rows*cols
      res = 0_int32
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj) reduction(+: res)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     res = res + matrix(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         res = sum(matrix)
      end if

   end function sum_matrix_int32

   function sum_matrix_int64(matrix) result(res)
      integer(int64), intent(in) :: matrix(:, :)
      integer(int64) :: res
      integer(default_int) :: cols, rows, n_elements, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      n_elements = rows*cols
      res = 0_int64
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj) reduction(+: res)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     res = res + matrix(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         res = sum(matrix)
      end if

   end function sum_matrix_int64

   function sum_matrix_sp(matrix) result(res)
      real(sp), intent(in) :: matrix(:, :)
      real(sp) :: res
      integer(default_int) :: cols, rows, n_elements, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      n_elements = rows*cols
      res = 0_sp
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj) reduction(+: res)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     res = res + matrix(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         res = sum(matrix)
      end if

   end function sum_matrix_sp

   function sum_matrix_dp(matrix) result(res)
      real(dp), intent(in) :: matrix(:, :)
      real(dp) :: res
      integer(default_int) :: cols, rows, n_elements, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      n_elements = rows*cols
      res = 0_dp
      if (n_elements > min_n_elements_parallel) then
         !$omp parallel do collapse(2) private(i,j,ii,jj) reduction(+: res)
         do jj = 1, cols, block_size
            do ii = 1, rows, block_size
               do j = jj, min(jj + block_size - 1, cols)
                  do i = ii, min(ii + block_size - 1, rows)
                     res = res + matrix(i, j)
                  end do
               end do
            end do
         end do
         !$omp end parallel do
      else
         res = sum(matrix)
      end if

   end function sum_matrix_dp

end module pic_array
