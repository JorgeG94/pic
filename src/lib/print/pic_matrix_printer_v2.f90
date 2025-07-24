!! This is v2 of the matrix printer module, a single interface for printing arrays of various types and in various formats
module pic_matrix_printer_v2
!! Generic module for printing arrays
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_string_utils, only: to_string, to_upper
   implicit none
   private

   public :: print_array_v2

   interface print_array_v2
    !! Generic interface for printing arrays of different types
    !! usage: call print_array(array) implemented types are:
    !! array(:)   -> int32, int64, sp, dp
    !! array(:,:) -> int32, int64, sp, dp
    !! array(:) (packed matrix) -> sp, dp
    !! array(:,:,:) -> sp, dp
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
      module procedure print_3d_tensor_sp
      module procedure print_3d_tensor_dp
   end interface print_array_v2

   character(len=5), parameter :: default_format = "NUMPY"
    !! supported formats: NUMPY, MATHEMATICA, and PLAIN which resembles numpy

   character(len=*), parameter :: fmt_edge = "(A)"
   character(len=*), parameter :: fmt_in = '(A, ", ")'

contains

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

   subroutine print_vector_int32(vector, format_type)
 !! print a vector of int32 values
      integer(int32), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, loop_bound_i
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
    !! print a vector of int64 values
      integer(int64), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int64) :: i, loop_bound_i
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
    !! print a vector of sp values
      real(sp), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, loop_bound_i
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
    !! print a vector of dp values
      real(dp), intent(in) :: vector(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, loop_bound_i
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
    !! print a matrix of int32 values
      integer(int32), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, rows, cols
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
    !! print a matrix of int64 values
      integer(int64), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, rows, cols
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
    !! print a matrix of sp values
      real(sp), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, rows, cols
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
    !! print a matrix of dp values
      real(dp), intent(in) :: matrix(:, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, rows, cols
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
    !! Print a packed lower triangular matrix of int32 values
      integer(int32), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type

      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(int32) :: n_real

      ! Determine format
      if (present(format_type)) then
         print_format = trim(adjustl(format_type))
      else
         print_format = "NUMPY"
      end if
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size
      n_real = (-1.0_int32 + sqrt(1.0_int32 + 8.0_int32*real(n_elements, int32)))/2.0_int32
      n = int(n_real + 0.5_int32)
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
    !! Print a packed lower triangular matrix of int64 values
      integer(int64), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type

      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(int64) :: n_real

      ! Determine format
      if (present(format_type)) then
         print_format = trim(adjustl(format_type))
      else
         print_format = "NUMPY"
      end if
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size
      n_real = (-1.0_int64 + sqrt(1.0_int64 + 8.0_int64*real(n_elements, int64)))/2.0_int64
      n = int(n_real + 0.5_int64)
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
    !! Print a packed lower triangular matrix of sp values
      real(sp), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type

      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(sp) :: n_real

      ! Determine format
      if (present(format_type)) then
         print_format = trim(adjustl(format_type))
      else
         print_format = "NUMPY"
      end if
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size
      n_real = (-1.0_sp + sqrt(1.0_sp + 8.0_sp*real(n_elements, sp)))/2.0_sp
      n = int(n_real + 0.5_sp)
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
    !! Print a packed lower triangular matrix of dp values
      real(dp), intent(in) :: packed(:)
      integer(default_int), intent(in) :: n_elements
      character(len=*), intent(in), optional :: format_type

      character(len=20) :: print_format
      character(len=1) :: open_bracket, close_bracket
      integer(default_int) :: i, j, idx, n
      real(dp) :: n_real

      ! Determine format
      if (present(format_type)) then
         print_format = trim(adjustl(format_type))
      else
         print_format = "NUMPY"
      end if
      call set_brackets(print_format, open_bracket, close_bracket)

      ! Compute n from packed size
      n_real = (-1.0_dp + sqrt(1.0_dp + 8.0_dp*real(n_elements, dp)))/2.0_dp
      n = int(n_real + 0.5_dp)
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

   subroutine print_3d_tensor_sp(matrix, format_type)
    !! Print a 3D tensor of sp values
      real(sp), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call print_array_v2(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_sp

   subroutine print_3d_tensor_dp(matrix, format_type)
    !! Print a 3D tensor of dp values
      real(dp), intent(in) :: matrix(:, :, :)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: print_format

      if (present(format_type)) then
         print_format = format_type
      else
         print_format = default_format
      end if

      print: block
         character(len=1) :: open_bracket, close_bracket
         integer(int32) :: i, j, k, rows, cols, depth
         rows = size(matrix, 1)
         cols = size(matrix, 2)
         depth = size(matrix, 3)
         call set_brackets(print_format, open_bracket, close_bracket)
         print *, open_bracket
         do k = 1, depth
            if (k > 1) print *, ","
            print *, open_bracket
            call print_array_v2(matrix(:, :, k), print_format)
            print *, close_bracket
         end do
         print *, close_bracket
      end block print

   end subroutine print_3d_tensor_dp

end module pic_matrix_printer_v2
