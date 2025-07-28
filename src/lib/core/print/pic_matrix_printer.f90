!! Routines to print vectors and matrices, currently no support for higher
!! dimensional arrays. Need to fix that!

module pic_matrix_printer
   !! Matrix printing module
   use pic_types, only: dp, default_int
   use pic_string_utils, only: to_string
   implicit none
   private
   public :: print_array, print_array_with_bounds
   interface print_array
     !! general interface for printing a one or two dimensional array
     !! will be deprecated
      module procedure print_vector
      module procedure print_matrix
   end interface print_array

   interface print_array_with_bounds
    !! general interface to print an array within certain bounds
      module procedure print_vector_n
      module procedure print_matrix_m_n
   end interface print_array_with_bounds

   character(len=*), parameter :: fmt_edge = "(F14.10)"
    !! no comma format
   character(len=*), parameter :: fmt_in = '(F14.10, ", ")'
    !! comma format for between arrays

contains

   subroutine print_vector_n(vec, n_elements, format_type)
    !! print a vector from start up to n_elements
      real(dp), intent(in) :: vec(:)
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      integer(kind=default_int), intent(in) :: n_elements
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = "PLAIN"
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == "PLAIN") then
         call print_plain_vector(vec, n_elements)
      else
         call print_vector_in_format(vec, format_selected, n_elements)
      end if
   end subroutine print_vector_n

   subroutine print_vector(vec, format_type)
    !! print a vector
      real(kind=dp), intent(in) :: vec(:)  ! 1D array
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected

      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = "PLAIN"
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == "PLAIN") then
         call print_plain_vector(vec)
      else
         call print_vector_in_format(vec, format_selected)
      end if
   end subroutine print_vector

   subroutine print_matrix_m_n(mat, n_cols, n_rows, format_type)
    !! Print a matrix of n_cols by n_rows
      real(kind=dp), intent(in) :: mat(:, :)  ! 2D array
      integer(kind=default_int), intent(in) :: n_cols, n_rows
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = "PLAIN"
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == "PLAIN") then
         call print_plain_matrix(mat, n_cols, n_rows)
      else
         call print_matrix_in_format(mat, format_selected, n_cols, n_rows)
      end if
   end subroutine print_matrix_m_n

   subroutine print_matrix(mat, format_type)
    !! print a matrix in a given format
      real(kind=dp), intent(in) :: mat(:, :)  ! 2D array
      character(len=*), intent(in), optional :: format_type
      character(len=20) :: format_selected
      ! Determine the format: default to "PLAIN" if not specified
      if (present(format_type)) then
         format_selected = trim(adjustl(format_type))
      else
         format_selected = "PLAIN"
      end if
      ! Handle plain format separately or delegate to print routine based on the format
      if (format_selected == "PLAIN") then
         call print_plain_matrix(mat)
      else
         call print_matrix_in_format(mat, format_selected)
      end if
   end subroutine print_matrix

   subroutine print_plain_vector(vec, n_elements)
    !! private subroutine that prints a vector of n_elements
      real(kind=dp), intent(in) :: vec(:)
      integer(kind=default_int), intent(in), optional :: n_elements
      integer(kind=default_int) :: i, loop_bound
      if (present(n_elements)) then
         loop_bound = n_elements
      else
         loop_bound = size(vec)
      end if
      print *, "Vector (Plain format):"
      do i = 1, loop_bound
         write (*, fmt_edge) vec(i)
      end do
   end subroutine print_plain_vector

   subroutine print_plain_matrix(mat, n_cols, n_rows)
    !! private subroutine that prints a plain matrix of n_cols by n_rows
      real(kind=dp), intent(in) :: mat(:, :)
      integer(kind=default_int), intent(in), optional :: n_cols, n_rows
      integer(kind=default_int) :: i, j, loop_bound_i, loop_bound_j
      if (present(n_cols) .and. present(n_rows)) then
         loop_bound_i = n_cols
         loop_bound_j = n_rows
      else
         loop_bound_i = size(mat, 1)
         loop_bound_j = size(mat, 2)
      end if
      print *, "Matrix (Plain format):"
      do i = 1, loop_bound_i
         do j = 1, loop_bound_j
            if (j == loop_bound_j) then
               write (*, fmt_edge, advance="yes") mat(i, j)  ! Last element in the row, new line
            else
               write (*, fmt_in, advance="no") mat(i, j)  ! In-between elements
            end if
         end do
      end do
   end subroutine print_plain_matrix

   subroutine print_vector_in_format(vec, format_type, n_elements)
    !! private subroutine that prints a vector in a format
      real(kind=dp), intent(in) :: vec(:)
      character(len=*), intent(in) :: format_type
        !! format can be mathematica or numpy
      integer(kind=default_int), intent(in), optional :: n_elements
      character(len=1) :: open_bracket, close_bracket
      integer(kind=default_int) :: i, loop_bound_i

      if (present(n_elements)) then
         loop_bound_i = n_elements
      else
         loop_bound_i = size(vec)
      end if
      ! Select brackets based on format type
      if (format_type == "NUMPY") then
         open_bracket = "["
         close_bracket = "]"
      else if (format_type == "MATHEMATICA") then
         open_bracket = "{"
         close_bracket = "}"
      else
         print *, "Error: Unsupported format type. Defaulting to NumPy format."
         open_bracket = "["
         close_bracket = "]"
      end if
      ! Print the vector in the selected format
      print *, "Vector (", trim(format_type), " format):"
      print *, open_bracket
      do i = 1, loop_bound_i
         if (i == loop_bound_i) then  ! Last element in the vector
            write (*, fmt_edge, advance="no") vec(i)
         else  ! Elements in between
            write (*, fmt_in, advance="no") vec(i)
         end if
      end do
      print *, close_bracket
   end subroutine print_vector_in_format

   subroutine print_matrix_in_format(mat, format_type, n_cols, n_rows)
    !! private subroutine to print a matrix in format
      real(kind=dp), intent(in) :: mat(:, :)
      character(len=*), intent(in) :: format_type
        !! format can be mathematica or numpy
      character(len=1) :: open_bracket, close_bracket
      integer(kind=default_int), intent(in), optional :: n_cols, n_rows
      integer(kind=default_int) :: i, j, loop_bound_i, loop_bound_j
      if (present(n_cols) .and. present(n_rows)) then
         loop_bound_i = n_cols
         loop_bound_j = n_rows
      else
         loop_bound_i = size(mat, 1)
         loop_bound_j = size(mat, 2)
      end if

      ! Select brackets based on format type
      if (format_type == "NUMPY") then
         open_bracket = "["
         close_bracket = "]"
      else if (format_type == "MATHEMATICA") then
         open_bracket = "{"
         close_bracket = "}"
      else
         print *, "Error: Unsupported format type. Defaulting to NumPy format."
         open_bracket = "["
         close_bracket = "]"
      end if

      ! Print the matrix in the selected format
      print *, "Matrix (", trim(format_type), " format):"
      print *, open_bracket
      do i = 1, loop_bound_i
         write (*, "(A)", advance="no") open_bracket  ! Start of a row
         do j = 1, loop_bound_j
            if (j == loop_bound_j) then  ! Last element in the row
               write (*, fmt_edge, advance="no") mat(i, j)
            else  ! Elements in between
               write (*, fmt_in, advance="no") mat(i, j)
            end if
         end do
         if (i == loop_bound_i) then
            print *, close_bracket  ! Close bracket without a comma for the last row
         else
            print *, close_bracket, ","  ! Close bracket with a comma for all other rows
         end if
      end do
      print *, close_bracket
   end subroutine print_matrix_in_format

end module pic_matrix_printer
