
!! pic array contains L0.5 BLAS level routines, as in things that could be use in
!! lieu of blas if you don't have it but if you do, please don't use these routines
module pic_array
!! Please do not modify this file to implement new methods, please go look at tools/autogen/pic_array_cpu.fypp
!! and edit the generator.
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_string, only: to_string, to_upper
   use pic_optional_value, only: pic_optional
   implicit none
   private

   public :: fill, copy
   public :: pic_transpose, pic_sum
   public :: pic_scramble_array, pic_print_array
   public :: is_sorted
   public :: set_threading_mode, get_threading_mode

   logical :: use_threaded_default = .false.
   public :: ASCENDING, DESCENDING

   integer(default_int), parameter :: ASCENDING = 1
   integer(default_int), parameter :: DESCENDING = 2
   character(len=5), parameter :: default_format = "NUMPY"
  !! supported formats: NUMPY, MATHEMATICA, and PLAIN which resembles numpy

   character(len=*), parameter :: fmt_edge = "(A)"
   character(len=*), parameter :: fmt_in = '(A, ", ")'

   interface set_threading_mode
   !! set_threading sets the threading mode for the array routines
   !! this will set the use_threaded variable to true or false depending on the input
   !! Usage: call set_threading_mode(.true.) or call set_threading_mode(.false.)
      module procedure set_threading_mode_
   end interface

   interface get_threading_mode
   !! get_threading_mode returns the current threading mode for the array routines
   !! Usage: mode = get_threading_mode()
      module procedure get_threading_mode_
   end interface get_threading_mode

   interface fill
  !! fill provides a generic interface to assing a value
  !! alpha of types (int32, int64, sp, dp) as defined in pic_types.F90
  !! The inteface supports filling 1d and 2d arrays of the specified
  !! variables
  !!
  !! Usage: call fill(array, value, [optional] threaded)
  !!
  !! This subroutine is threaded for performance purposes if threaded is set to .true.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
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
  !! if you built pic with BLAS use the copy interface provided there, I will not beat BLAS
  !! copy is implemented for (int32, int64, sp, dp) for 1 and 2d arrays of the same types
  !!
  !! Usage: call copy(destination, source, [optional] threaded)
  !!
  !! This subroutine is threaded for performance purposes if threaded is set to .true.
  !!
  !! @note If this subroutine is called inside a omp threaded region it will run serially because of nested parallelism
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
  !! Usage: call pic_transpose(matrix_to_transpose, result, [optional] threaded)
  !!
  !! This subroutine is threaded for performance purposes if threaded is set to true
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
  !! Usage: result = pic_sum(array, [optional] threaded)
  !!
  !! This subroutine is threaded for performance purposes if threaded is set to true
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
   end interface

   interface is_sorted
    !! is_sorted provides a simple way to checking if a 1d array is sorted
    !! it is implemented for int32, int64, sp, and dp datatypes. The default
    !! is to check if an array is sorted in ascending fashion.
    !!
    !! Usage: result = is_sorted(array, [optional] ASCENDING/DESCENDING)
      module procedure is_sorted_int32
      module procedure is_sorted_int64
      module procedure is_sorted_sp
      module procedure is_sorted_dp
      module procedure is_sorted_char
   end interface

   interface pic_print_array
    !! Generic interface for printing arrays of different types
    !!
    !! Usage: call print_array(array, [optional] format)
    !! Where format can be: NUMPY, PLAIN, MATHEMATICA (can use lower caps)
    !!
    !! Implemented types are:
    !!
    !! array(:)   -> int32, int64, sp, dp
    !!
    !! array(:,:) -> int32, int64, sp, dp
    !!
    !! array(:) (packed matrix) -> sp, dp
    !!
    !! array(:,:,:) -> sp, dp
    !!
      module procedure print_vector_int32
      module procedure print_vector_int64
      module procedure print_vector_sp
      module procedure print_vector_dp
      module procedure print_matrix_int32
      module procedure print_matrix_int64
      module procedure print_matrix_sp
      module procedure print_matrix_dp
      module procedure print_packed_matrix_int32
      module procedure print_packed_matrix_int64
      module procedure print_packed_matrix_sp
      module procedure print_packed_matrix_dp
      module procedure print_3d_tensor_int32
      module procedure print_3d_tensor_int64
      module procedure print_3d_tensor_sp
      module procedure print_3d_tensor_dp

   end interface

   interface pic_scramble_array
      module procedure scramble_array_int32
      module procedure scramble_array_int64
      module procedure scramble_array_sp
      module procedure scramble_array_dp
      module procedure scramble_array_character
   end interface pic_scramble_array

   ! potentially implement a shallow copy? nah?
   integer(default_int), parameter :: block_size = 32
    !! This is the size to block over for matrices for performance purposes

contains

   subroutine set_threading_mode_(threaded)
      !! set the threading mode for the array routines, this will set the use_threaded variable
      !! to true or false depending on the input
      !!
      !! Usage: call set_threading(.true.) or call set_threading(.false.)
      logical, intent(in) :: threaded
      use_threaded_default = threaded
   end subroutine set_threading_mode_

   function get_threading_mode_() result(mode)
      !! get the current threading mode for the array routines
      !! Usage: mode = get_threading_mode()
      logical :: mode
      mode = use_threaded_default
   end function get_threading_mode_

   subroutine set_brackets(format_type, open_bracket, close_bracket)
   !! Set brackets based on output format type
      character(len=*), intent(in) :: format_type
      character(len=1), intent(out) :: open_bracket, close_bracket
      select case (trim(to_upper(adjustl(format_type))))
      case ("NUMPY")
         open_bracket = "["
         close_bracket = "]"
      case ("MATHEMATICA")
         open_bracket = "{"
         close_bracket = "}"
      case ("PLAIN")
         open_bracket = "["
         close_bracket = "]"
      case default
         print *, "Warning: Unsupported format type '"//trim(format_type)//"'. Defaulting to NumPy style."
         open_bracket = "["
         close_bracket = "]"
      end select
   end subroutine set_brackets

   subroutine fill_vector_int32(vector, alpha, threaded)
      integer(int32), intent(inout) :: vector(:)
      integer(int32), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i

      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_int32

   subroutine fill_vector_int64(vector, alpha, threaded)
      integer(int64), intent(inout) :: vector(:)
      integer(int64), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i

      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_int64

   subroutine fill_vector_sp(vector, alpha, threaded)
      real(sp), intent(inout) :: vector(:)
      real(sp), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i

      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_sp

   subroutine fill_vector_dp(vector, alpha, threaded)
      real(dp), intent(inout) :: vector(:)
      real(dp), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i

      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_dp

   subroutine fill_matrix_int32(matrix, alpha, threaded)
      integer(int32), intent(inout) :: matrix(:, :)
      integer(int32), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      logical, intent(in), optional :: threaded
      logical :: use_threads
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine fill_matrix_int64(matrix, alpha, threaded)
      integer(int64), intent(inout) :: matrix(:, :)
      integer(int64), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      logical, intent(in), optional :: threaded
      logical :: use_threads
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine fill_matrix_sp(matrix, alpha, threaded)
      real(sp), intent(inout) :: matrix(:, :)
      real(sp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      logical, intent(in), optional :: threaded
      logical :: use_threads
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine fill_matrix_dp(matrix, alpha, threaded)
      real(dp), intent(inout) :: matrix(:, :)
      real(dp), intent(in)    :: alpha
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      logical, intent(in), optional :: threaded
      logical :: use_threads
      rows = size(matrix, 1)
      cols = size(matrix, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine copy_vector_int32(dest, source, threaded)
      integer(int32), intent(inout) :: dest(:)
      integer(int32), intent(in)    :: source(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_int32

   subroutine copy_vector_int64(dest, source, threaded)
      integer(int64), intent(inout) :: dest(:)
      integer(int64), intent(in)    :: source(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_int64

   subroutine copy_vector_sp(dest, source, threaded)
      real(sp), intent(inout) :: dest(:)
      real(sp), intent(in)    :: source(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_sp

   subroutine copy_vector_dp(dest, source, threaded)
      real(dp), intent(inout) :: dest(:)
      real(dp), intent(in)    :: source(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i
      if (size(dest, 1) /= size(source, 1)) then
         error stop "Vector size mismatch"
      end if
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(dest, 1)
            dest(i) = source(i)
         end do
         !$omp end parallel do
      else
         dest = source
      end if
   end subroutine copy_vector_dp

   subroutine copy_matrix_int32(dest, source, threaded)
      integer(int32), intent(inout) :: dest(:, :)
      integer(int32), intent(in)    :: source(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine copy_matrix_int64(dest, source, threaded)
      integer(int64), intent(inout) :: dest(:, :)
      integer(int64), intent(in)    :: source(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine copy_matrix_sp(dest, source, threaded)
      real(sp), intent(inout) :: dest(:, :)
      real(sp), intent(in)    :: source(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine copy_matrix_dp(dest, source, threaded)
      real(dp), intent(inout) :: dest(:, :)
      real(dp), intent(in)    :: source(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, rows, cols
      integer(default_int) :: ii, jj
      if (size(dest, 1) /= size(source, 1) .or. size(dest, 2) /= size(source, 2)) then
         error stop "Matrix size mismatch"
      end if
      rows = size(source, 1)
      cols = size(source, 2)
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
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

   subroutine transpose_matrix_int32(A, B, threaded)
      integer(int32), intent(in)  :: A(:, :)
      integer(int32), intent(out) :: B(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      use_threads = pic_optional(threaded, use_threaded_default)

      if (use_threads) then
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

   subroutine transpose_matrix_int64(A, B, threaded)
      integer(int64), intent(in)  :: A(:, :)
      integer(int64), intent(out) :: B(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      use_threads = pic_optional(threaded, use_threaded_default)

      if (use_threads) then
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

   subroutine transpose_matrix_sp(A, B, threaded)
      real(sp), intent(in)  :: A(:, :)
      real(sp), intent(out) :: B(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      use_threads = pic_optional(threaded, use_threaded_default)

      if (use_threads) then
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

   subroutine transpose_matrix_dp(A, B, threaded)
      real(dp), intent(in)  :: A(:, :)
      real(dp), intent(out) :: B(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i, j, ii, jj, rows, cols

      rows = size(A, 1)
      cols = size(A, 2)

      if (size(B, 1) /= cols .or. size(B, 2) /= rows) then
         error stop "transpose: size mismatch"
      end if

      use_threads = pic_optional(threaded, use_threaded_default)

      if (use_threads) then
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

   function sum_vector_int32(vector, threaded) result(res)
      integer(int32), intent(in)  :: vector(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(int32) :: res
      integer(default_int) :: i
      res = 0_int32
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, size(vector, 1)
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_int32
   function sum_vector_int64(vector, threaded) result(res)
      integer(int64), intent(in)  :: vector(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(int64) :: res
      integer(default_int) :: i
      res = 0_int64
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, size(vector, 1)
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_int64
   function sum_vector_sp(vector, threaded) result(res)
      real(sp), intent(in)  :: vector(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      real(sp) :: res
      integer(default_int) :: i
      res = 0_sp
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, size(vector, 1)
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_sp
   function sum_vector_dp(vector, threaded) result(res)
      real(dp), intent(in)  :: vector(:)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      real(dp) :: res
      integer(default_int) :: i
      res = 0_dp
      use_threads = pic_optional(threaded, use_threaded_default)
      if (use_threads) then
         !$omp parallel do private(i) collapse(1) reduction(+:res)
         do i = 1, size(vector, 1)
            res = res + vector(i)
         end do
         !$omp end parallel do
      else
         res = sum(vector)
      end if
   end function sum_vector_dp

   function sum_matrix_int32(matrix, threaded) result(res)
      integer(int32), intent(in) :: matrix(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(int32) :: res
      integer(default_int) :: cols, rows, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      use_threads = pic_optional(threaded, use_threaded_default)
      res = 0_int32
      if (use_threads) then
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

   function sum_matrix_int64(matrix, threaded) result(res)
      integer(int64), intent(in) :: matrix(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(int64) :: res
      integer(default_int) :: cols, rows, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      use_threads = pic_optional(threaded, use_threaded_default)
      res = 0_int64
      if (use_threads) then
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

   function sum_matrix_sp(matrix, threaded) result(res)
      real(sp), intent(in) :: matrix(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      real(sp) :: res
      integer(default_int) :: cols, rows, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      use_threads = pic_optional(threaded, use_threaded_default)
      res = 0_sp
      if (use_threads) then
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

   function sum_matrix_dp(matrix, threaded) result(res)
      real(dp), intent(in) :: matrix(:, :)
      logical, intent(in), optional :: threaded
      logical :: use_threads
      real(dp) :: res
      integer(default_int) :: cols, rows, i, j, ii, jj

      rows = size(matrix, 1)
      cols = size(matrix, 2)

      use_threads = pic_optional(threaded, use_threaded_default)
      res = 0_dp
      if (use_threads) then
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

   pure function is_sorted_int32(array, order) result(sorted)
      integer(int32), intent(in) :: array(:)
      integer(default_int), intent(in), optional :: order
      integer(default_int):: sort_order
      integer(default_int) :: i
      logical :: sorted

      sorted = .true.
      sort_order = pic_optional(order, ASCENDING)

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select

   end function is_sorted_int32

   pure function is_sorted_int64(array, order) result(sorted)
      integer(int64), intent(in) :: array(:)
      integer(default_int), intent(in), optional :: order
      integer(default_int):: sort_order
      integer(default_int) :: i
      logical :: sorted

      sorted = .true.
      sort_order = pic_optional(order, ASCENDING)

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select

   end function is_sorted_int64

   pure function is_sorted_sp(array, order) result(sorted)
      real(sp), intent(in) :: array(:)
      integer(default_int), intent(in), optional :: order
      integer(default_int):: sort_order
      integer(default_int) :: i
      logical :: sorted

      sorted = .true.
      sort_order = pic_optional(order, ASCENDING)

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select

   end function is_sorted_sp

   pure function is_sorted_dp(array, order) result(sorted)
      real(dp), intent(in) :: array(:)
      integer(default_int), intent(in), optional :: order
      integer(default_int):: sort_order
      integer(default_int) :: i
      logical :: sorted

      sorted = .true.
      sort_order = pic_optional(order, ASCENDING)

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select

   end function is_sorted_dp

   pure function is_sorted_char(array, order) result(sorted)
      character(len=*), intent(in) :: array(:)
      integer(default_int), intent(in), optional :: order
      integer(default_int) :: sort_order
      integer(default_int) :: i
      logical :: sorted

      sorted = .true.
      sort_order = pic_optional(order, ASCENDING)

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select
   end function is_sorted_char

   subroutine print_vector_int32(vector, format_type)
     !! print a vector of ${T} values
      integer(int32), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, loop_bound_i
         loop_bound_i = size(vector)
         call set_brackets(print_format, open_bracket, close_bracket)
         write (*, "(A)", advance="no") open_bracket
         do i = 1, loop_bound_i
            if (i == loop_bound_i) then  ! Last element in the vector
               write (*, fmt_edge, advance="no") to_string(vector(i))
            else  ! Elements in between
               write (*, fmt_in, advance="no") to_string(vector(i))
            end if
         end do
         print *, close_bracket

      end block print

   end subroutine print_vector_int32

   subroutine print_vector_int64(vector, format_type)
     !! print a vector of ${T} values
      integer(int64), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, loop_bound_i
         loop_bound_i = size(vector)
         call set_brackets(print_format, open_bracket, close_bracket)
         write (*, "(A)", advance="no") open_bracket
         do i = 1, loop_bound_i
            if (i == loop_bound_i) then  ! Last element in the vector
               write (*, fmt_edge, advance="no") to_string(vector(i))
            else  ! Elements in between
               write (*, fmt_in, advance="no") to_string(vector(i))
            end if
         end do
         print *, close_bracket

      end block print

   end subroutine print_vector_int64

   subroutine print_vector_sp(vector, format_type)
     !! print a vector of ${T} values
      real(sp), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, loop_bound_i
         loop_bound_i = size(vector)
         call set_brackets(print_format, open_bracket, close_bracket)
         write (*, "(A)", advance="no") open_bracket
         do i = 1, loop_bound_i
            if (i == loop_bound_i) then  ! Last element in the vector
               write (*, fmt_edge, advance="no") to_string(vector(i))
            else  ! Elements in between
               write (*, fmt_in, advance="no") to_string(vector(i))
            end if
         end do
         print *, close_bracket

      end block print

   end subroutine print_vector_sp

   subroutine print_vector_dp(vector, format_type)
     !! print a vector of ${T} values
      real(dp), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, loop_bound_i
         loop_bound_i = size(vector)
         call set_brackets(print_format, open_bracket, close_bracket)
         write (*, "(A)", advance="no") open_bracket
         do i = 1, loop_bound_i
            if (i == loop_bound_i) then  ! Last element in the vector
               write (*, fmt_edge, advance="no") to_string(vector(i))
            else  ! Elements in between
               write (*, fmt_in, advance="no") to_string(vector(i))
            end if
         end do
         print *, close_bracket

      end block print

   end subroutine print_vector_dp

   subroutine print_matrix_int32(matrix, format_type)
    !! print a matrix of ${T} values
      integer(int32), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, j, rows, cols
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do i = 1, rows
            write (*, "(A)", advance="no") open_bracket
            do j = 1, cols
               if (j == cols) then  ! Last element in the row
                  write (*, fmt_edge, advance="no") to_string(matrix(i, j))
               else  ! Elements in between
                  write (*, fmt_in, advance="no") to_string(matrix(i, j))
               end if
            end do
            if (i == rows) then
               print *, close_bracket
            else
               print *, close_bracket, ","
            end if
         end do
         print *, close_bracket
      end block print

   end subroutine print_matrix_int32

   subroutine print_matrix_int64(matrix, format_type)
    !! print a matrix of ${T} values
      integer(int64), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, j, rows, cols
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do i = 1, rows
            write (*, "(A)", advance="no") open_bracket
            do j = 1, cols
               if (j == cols) then  ! Last element in the row
                  write (*, fmt_edge, advance="no") to_string(matrix(i, j))
               else  ! Elements in between
                  write (*, fmt_in, advance="no") to_string(matrix(i, j))
               end if
            end do
            if (i == rows) then
               print *, close_bracket
            else
               print *, close_bracket, ","
            end if
         end do
         print *, close_bracket
      end block print

   end subroutine print_matrix_int64

   subroutine print_matrix_sp(matrix, format_type)
    !! print a matrix of ${T} values
      real(sp), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, j, rows, cols
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do i = 1, rows
            write (*, "(A)", advance="no") open_bracket
            do j = 1, cols
               if (j == cols) then  ! Last element in the row
                  write (*, fmt_edge, advance="no") to_string(matrix(i, j))
               else  ! Elements in between
                  write (*, fmt_in, advance="no") to_string(matrix(i, j))
               end if
            end do
            if (i == rows) then
               print *, close_bracket
            else
               print *, close_bracket, ","
            end if
         end do
         print *, close_bracket
      end block print

   end subroutine print_matrix_sp

   subroutine print_matrix_dp(matrix, format_type)
    !! print a matrix of ${T} values
      real(dp), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(default_int) :: i, j, rows, cols
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do i = 1, rows
            write (*, "(A)", advance="no") open_bracket
            do j = 1, cols
               if (j == cols) then  ! Last element in the row
                  write (*, fmt_edge, advance="no") to_string(matrix(i, j))
               else  ! Elements in between
                  write (*, fmt_in, advance="no") to_string(matrix(i, j))
               end if
            end do
            if (i == rows) then
               print *, close_bracket
            else
               print *, close_bracket, ","
            end if
         end do
         print *, close_bracket
      end block print

   end subroutine print_matrix_dp

   subroutine print_packed_matrix_int32(packed, n_elements, format_type)
   !! Print a packed lower triangular matrix of ${T} values
      integer(int32), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(dp) :: n_real

      ! Determine format
      print_format = pic_optional(format_type, default_format)
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size using proper real arithmetic
      n_real = (-1.0_dp + sqrt(1.0_dp + 8.0_dp*real(n_elements, dp)))/2.0_dp
      n = int(n_real + 0.5_dp, default_int)

      if (n*(n + 1)/2 /= n_elements) then
         print *, "Error: n_elements does not form a valid packed triangle"
         return
      end if

      ! Print lower triangle directly from packed array
      print *, open_bracket
      idx = 0
      do i = 1, n
         write (*, '(A)', advance="no") open_bracket
         do j = 1, i
            idx = idx + 1
            if (j == i) then
               write (*, '(A)', advance="no") to_string(packed(idx))
            else
               write (*, '(A)', advance="no") trim(to_string(packed(idx))//", ")
            end if
         end do
         if (i == n) then
            print *, close_bracket
         else
            print *, close_bracket, ","
         end if
      end do
      print *, close_bracket
   end subroutine print_packed_matrix_int32

   subroutine print_packed_matrix_int64(packed, n_elements, format_type)
   !! Print a packed lower triangular matrix of ${T} values
      integer(int64), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(dp) :: n_real

      ! Determine format
      print_format = pic_optional(format_type, default_format)
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size using proper real arithmetic
      n_real = (-1.0_dp + sqrt(1.0_dp + 8.0_dp*real(n_elements, dp)))/2.0_dp
      n = int(n_real + 0.5_dp, default_int)

      if (n*(n + 1)/2 /= n_elements) then
         print *, "Error: n_elements does not form a valid packed triangle"
         return
      end if

      ! Print lower triangle directly from packed array
      print *, open_bracket
      idx = 0
      do i = 1, n
         write (*, '(A)', advance="no") open_bracket
         do j = 1, i
            idx = idx + 1
            if (j == i) then
               write (*, '(A)', advance="no") to_string(packed(idx))
            else
               write (*, '(A)', advance="no") trim(to_string(packed(idx))//", ")
            end if
         end do
         if (i == n) then
            print *, close_bracket
         else
            print *, close_bracket, ","
         end if
      end do
      print *, close_bracket
   end subroutine print_packed_matrix_int64

   subroutine print_packed_matrix_sp(packed, n_elements, format_type)
   !! Print a packed lower triangular matrix of ${T} values
      real(sp), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(dp) :: n_real

      ! Determine format
      print_format = pic_optional(format_type, default_format)
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size using proper real arithmetic
      n_real = (-1.0_dp + sqrt(1.0_dp + 8.0_dp*real(n_elements, dp)))/2.0_dp
      n = int(n_real + 0.5_dp, default_int)

      if (n*(n + 1)/2 /= n_elements) then
         print *, "Error: n_elements does not form a valid packed triangle"
         return
      end if

      ! Print lower triangle directly from packed array
      print *, open_bracket
      idx = 0
      do i = 1, n
         write (*, '(A)', advance="no") open_bracket
         do j = 1, i
            idx = idx + 1
            if (j == i) then
               write (*, '(A)', advance="no") to_string(packed(idx))
            else
               write (*, '(A)', advance="no") trim(to_string(packed(idx))//", ")
            end if
         end do
         if (i == n) then
            print *, close_bracket
         else
            print *, close_bracket, ","
         end if
      end do
      print *, close_bracket
   end subroutine print_packed_matrix_sp

   subroutine print_packed_matrix_dp(packed, n_elements, format_type)
   !! Print a packed lower triangular matrix of ${T} values
      real(dp), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(dp) :: n_real

      ! Determine format
      print_format = pic_optional(format_type, default_format)
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size using proper real arithmetic
      n_real = (-1.0_dp + sqrt(1.0_dp + 8.0_dp*real(n_elements, dp)))/2.0_dp
      n = int(n_real + 0.5_dp, default_int)

      if (n*(n + 1)/2 /= n_elements) then
         print *, "Error: n_elements does not form a valid packed triangle"
         return
      end if

      ! Print lower triangle directly from packed array
      print *, open_bracket
      idx = 0
      do i = 1, n
         write (*, '(A)', advance="no") open_bracket
         do j = 1, i
            idx = idx + 1
            if (j == i) then
               write (*, '(A)', advance="no") to_string(packed(idx))
            else
               write (*, '(A)', advance="no") trim(to_string(packed(idx))//", ")
            end if
         end do
         if (i == n) then
            print *, close_bracket
         else
            print *, close_bracket, ","
         end if
      end do
      print *, close_bracket
   end subroutine print_packed_matrix_dp

   subroutine print_3d_tensor_int32(matrix, format_type)
    !! Print a 3D tensor of ${T} values
      integer(int32), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call pic_print_array(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_int32

   subroutine print_3d_tensor_int64(matrix, format_type)
    !! Print a 3D tensor of ${T} values
      integer(int64), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call pic_print_array(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_int64

   subroutine print_3d_tensor_sp(matrix, format_type)
    !! Print a 3D tensor of ${T} values
      real(sp), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call pic_print_array(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_sp

   subroutine print_3d_tensor_dp(matrix, format_type)
    !! Print a 3D tensor of ${T} values
      real(dp), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      print_format = pic_optional(format_type, default_format)

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call pic_print_array(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_dp

   subroutine scramble_array_int32(array)
      integer(int32), intent(inout) :: array(:)
      integer(int32) :: i, j, n
      integer(int32) :: temp
      real(sp) :: rand_val

      n = size(array)
      do i = n, 2, -1
         call random_number(rand_val)
         j = int(rand_val*i) + 1
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
      end do
   end subroutine scramble_array_int32

   subroutine scramble_array_int64(array)
      integer(int64), intent(inout) :: array(:)
      integer(int32) :: i, j, n
      integer(int64) :: temp
      real(sp) :: rand_val

      n = size(array)
      do i = n, 2, -1
         call random_number(rand_val)
         j = int(rand_val*i) + 1
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
      end do
   end subroutine scramble_array_int64

   subroutine scramble_array_sp(array)
      real(sp), intent(inout) :: array(:)
      integer(int32) :: i, j, n
      real(sp) :: temp
      real(sp) :: rand_val

      n = size(array)
      do i = n, 2, -1
         call random_number(rand_val)
         j = int(rand_val*i) + 1
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
      end do
   end subroutine scramble_array_sp

   subroutine scramble_array_dp(array)
      real(dp), intent(inout) :: array(:)
      integer(int32) :: i, j, n
      real(dp) :: temp
      real(sp) :: rand_val

      n = size(array)
      do i = n, 2, -1
         call random_number(rand_val)
         j = int(rand_val*i) + 1
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
      end do
   end subroutine scramble_array_dp

   subroutine scramble_array_character(array)
      character(len=*), intent(inout) :: array(:)
      integer(int32) :: i, j, n
      character(len=len(array)) :: temp
      real(sp) :: rand_val

      n = size(array)
      do i = n, 2, -1
         call random_number(rand_val)
         j = int(rand_val*i) + 1
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
      end do
   end subroutine scramble_array_character
end module pic_array
