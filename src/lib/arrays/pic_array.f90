
module pic_array
   use pic_types, only: sp, dp, int32, int64, default_int
   implicit none
   private

   public :: fill, copy, pic_transpose

   interface fill
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
      module procedure transpose_matrix_int32
      module procedure transpose_matrix_int64
      module procedure transpose_matrix_sp
      module procedure transpose_matrix_dp
   end interface

   ! potentially implement a shallow copy? nah?
   integer(default_int), parameter :: block_size = 32

contains

   subroutine fill_vector_int32(vector, alpha)
      integer(int32), intent(inout) :: vector(:)
      integer(int32), intent(in)    :: alpha
      integer(default_int) :: i
      !$omp target teams loop collapse(1)
      do i = 1, size(vector, 1)
         vector(i) = alpha
      end do
      !$omp end target teams loop
   end subroutine fill_vector_int32

   subroutine fill_vector_int64(vector, alpha)
      integer(int64), intent(inout) :: vector(:)
      integer(int64), intent(in)    :: alpha
      integer(default_int) :: i
      !$omp target teams loop collapse(1)
      do i = 1, size(vector, 1)
         vector(i) = alpha
      end do
      !$omp end target teams loop
   end subroutine fill_vector_int64

   subroutine fill_vector_sp(vector, alpha)
      real(sp), intent(inout) :: vector(:)
      real(sp), intent(in)    :: alpha
      integer(default_int) :: i
      !$omp target teams loop collapse(1)
      do i = 1, size(vector, 1)
         vector(i) = alpha
      end do
      !$omp end target teams loop
   end subroutine fill_vector_sp

   subroutine fill_vector_dp(vector, alpha)
      real(dp), intent(inout) :: vector(:)
      real(dp), intent(in)    :: alpha
      integer(default_int) :: i
      !$omp target teams loop collapse(1)
      do i = 1, size(vector, 1)
         vector(i) = alpha
      end do
      !$omp end target teams loop
   end subroutine fill_vector_dp

   subroutine fill_matrix_int32(matrix, alpha)
      integer(int32), intent(inout) :: matrix(:, :)
      integer(int32), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  matrix(i, j) = alpha
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine fill_matrix_int32

   subroutine fill_matrix_int64(matrix, alpha)
      integer(int64), intent(inout) :: matrix(:, :)
      integer(int64), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  matrix(i, j) = alpha
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine fill_matrix_int64

   subroutine fill_matrix_sp(matrix, alpha)
      real(sp), intent(inout) :: matrix(:, :)
      real(sp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  matrix(i, j) = alpha
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine fill_matrix_sp

   subroutine fill_matrix_dp(matrix, alpha)
      real(dp), intent(inout) :: matrix(:, :)
      real(dp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  matrix(i, j) = alpha
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine fill_matrix_dp

   subroutine copy_vector_int32(dest, source)
      integer(int32), intent(inout) :: dest(:)
      integer(int32), intent(in)    :: source(:)
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      !$omp target teams loop collapse(1)
      do i = 1, size(dest, 1)
         dest(i) = source(i)
      end do
      !$omp end target teams loop
   end subroutine copy_vector_int32

   subroutine copy_vector_int64(dest, source)
      integer(int64), intent(inout) :: dest(:)
      integer(int64), intent(in)    :: source(:)
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      !$omp target teams loop collapse(1)
      do i = 1, size(dest, 1)
         dest(i) = source(i)
      end do
      !$omp end target teams loop
   end subroutine copy_vector_int64

   subroutine copy_vector_sp(dest, source)
      real(sp), intent(inout) :: dest(:)
      real(sp), intent(in)    :: source(:)
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      !$omp target teams loop collapse(1)
      do i = 1, size(dest, 1)
         dest(i) = source(i)
      end do
      !$omp end target teams loop
   end subroutine copy_vector_sp

   subroutine copy_vector_dp(dest, source)
      real(dp), intent(inout) :: dest(:)
      real(dp), intent(in)    :: source(:)
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      !$omp target teams loop collapse(1)
      do i = 1, size(dest, 1)
         dest(i) = source(i)
      end do
      !$omp end target teams loop
   end subroutine copy_vector_dp

   subroutine copy_matrix_int32(dest, source)
      integer(int32), intent(inout) :: dest(:, :)
      integer(int32), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  dest(i, j) = source(i, j)
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine copy_matrix_int32

   subroutine copy_matrix_int64(dest, source)
      integer(int64), intent(inout) :: dest(:, :)
      integer(int64), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  dest(i, j) = source(i, j)
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine copy_matrix_int64

   subroutine copy_matrix_sp(dest, source)
      real(sp), intent(inout) :: dest(:, :)
      real(sp), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  dest(i, j) = source(i, j)
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine copy_matrix_sp

   subroutine copy_matrix_dp(dest, source)
      real(dp), intent(inout) :: dest(:, :)
      real(dp), intent(in)    :: source(:, :)
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      !$omp target teams loop collapse(2) private(i,j,ii,jj)
      do jj = 1, cols, block_size
         do ii = 1, rows, block_size
            do j = jj, min(jj + block_size - 1, cols)
               do i = ii, min(ii + block_size - 1, rows)
                  dest(i, j) = source(i, j)
               end do
            end do
         end do
      end do
      !$omp end target teams loop
   end subroutine copy_matrix_dp

   subroutine transpose_matrix_int32(A, B)
      integer(int32), intent(in)  :: A(:, :)
      integer(int32), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

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
   end subroutine transpose_matrix_int32

   subroutine transpose_matrix_int64(A, B)
      integer(int64), intent(in)  :: A(:, :)
      integer(int64), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

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
   end subroutine transpose_matrix_int64

   subroutine transpose_matrix_sp(A, B)
      real(sp), intent(in)  :: A(:, :)
      real(sp), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

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
   end subroutine transpose_matrix_sp

   subroutine transpose_matrix_dp(A, B)
      real(dp), intent(in)  :: A(:, :)
      real(dp), intent(out) :: B(:, :)
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

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
   end subroutine transpose_matrix_dp

end module pic_array
