!! Life is easier when we have strings. This file
!! contains the necessary routines to transform key data
!! types into strings

module pic_string
!! General string utilities
   use pic_types, only: sp, dp, int32, int64, default_int
   implicit none
   ! Generic interface for to_string to handle different types
   private
   integer(default_int), parameter :: default_dp_precision = 12
   integer(default_int) :: dp_precision = default_dp_precision
   integer(default_int), parameter :: default_sp_precision = 6
   integer(default_int) :: sp_precision = default_sp_precision

   public :: to_string, pad, to_upper
   public :: set_precision, get_precision

   interface to_string
      !! converts a variable of type (int32, int64, sp, dp, char, logical)
      !! to a "string" which is just a collecting of chars.
      !!
      !! Usage result = to_string(variable)
      !!
      !! @note the functions here are not elemental so they won't work for
      !! arrays. Please use pic_print_array_v2 module for this
      !!
      module procedure to_string_int32
      module procedure to_string_int64
      module procedure to_string_sp
      module procedure to_string_dp
      module procedure to_string_char
      module procedure to_string_logical
      module procedure to_string_vector_int32
      module procedure to_string_vector_int64
      module procedure to_string_vector_sp
      module procedure to_string_vector_dp
      module procedure to_string_matrix_int32
      module procedure to_string_matrix_int64
      module procedure to_string_matrix_sp
      module procedure to_string_matrix_dp
   end interface

   interface to_upper
    !! takes a character variable and transforms it to uppercase
    !!
    !! usage var = to_upper("hello")
    !!
      module procedure to_upper
   end interface

   interface pad
    !! adds a number X of spaces to the left of a "string" whcih is just a
    !! collection of characters. Mostly used for nice printing
    !!
    !! Usage: var = pad("hello", n_spaces)
    !!
      module procedure pad
   end interface

   interface set_precision
    !! This routine overrides the default dp precision used for
    !! printing strings in the to_string function, the default
    !! is : integer(default_int), parameter :: default_dp_precision = 12
    !!
    !! Usage: call set_precision(variable) where variable is default_int
    !!
      module procedure set_precision
   end interface

   interface get_precision
    !! Obtain the current precision being used to print variables to strings
    !!
    !! Usage: precision = get_precision()
    !!
    !! returns a default_int result
      module procedure get_precision
   end interface

contains

   function to_upper(str) result(upper_str)
      character(len=*), intent(in) :: str
      character(len=len(str)) :: upper_str
      integer(default_int) :: i
      character :: ch

      do i = 1, len(str)
         ch = str(i:i)
         if (ch >= 'a' .and. ch <= 'z') then
            upper_str(i:i) = char(iachar(ch) - 32)
         else
            upper_str(i:i) = ch
         end if
      end do
   end function to_upper

   function pad(s, width) result(padded)
    !! function to pad a string with a certain number of characters for nice printing
      character(len=*), intent(in) :: s
      integer(default_int), intent(in) :: width
      character(len=:), allocatable :: padded
      integer(default_int) :: len_s

      len_s = len_trim(s)
      if (len_s >= width) then
         padded = s(1:width)
      else
         padded = repeat(" ", width - len_s)//s
      end if
   end function pad

   subroutine set_precision(precision)
      !! Set the precision for real numbers
      integer(default_int), intent(in) :: precision
      if (precision > 0) then
         dp_precision = precision
      else
         print *, "Warning: Precision must be positive. Using default."
         dp_precision = default_dp_precision
      end if
   end subroutine set_precision

   function get_precision() result(precision)
      !! Get the current precision for real numbers
      integer(default_int) :: precision
      precision = dp_precision
   end function get_precision

   function to_string_int32(i) result(trimmed_str)
      !! transform an int32 to a string
      integer(kind=int32), intent(in) :: i
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, "(I0)") i  ! Convert integer to string without leading spaces
      trimmed_str = trim(str)
   end function to_string_int32

   function to_string_int64(i) result(trimmed_str)
      !! transform an int64 to a string
      integer(kind=int64), intent(in) :: i
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, "(I0)") i  ! Convert integer to string without leading spaces
      trimmed_str = trim(str)
   end function to_string_int64

   function to_string_sp(r) result(trimmed_str)
      !! transform a real(sp) to a string
      real(kind=sp), intent(in) :: r
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      character(len=32) :: style
      !call write_with_precision(r, str)
      write (style, '(A,I0,A)') '(F0.', dp_precision, ')'
      write (str, style) r
      trimmed_str = trim(str)
   end function to_string_sp

   function to_string_dp(r) result(trimmed_str)
      !! transform a real(dp) to a string
      real(kind=dp), intent(in) :: r
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      character(len=32) :: style
      !call write_with_precision(r, str)
      write (style, '(A,I0,A)') '(F0.', dp_precision, ')'
      write (str, style) r
      trimmed_str = trim(str)
   end function to_string_dp

   function to_string_char(c) result(trimmed_str)
      !! transform a character to a string
      character(len=*), intent(in) :: c
      character(len=500) :: str
      character(len=:), allocatable :: trimmed_str
      str = c
      trimmed_str = trim(str)
   end function to_string_char

   function to_string_logical(l) result(trimmed_str)
      !! tranform a logical to a string either true or false
      logical, intent(in) :: l
      character(len=5) :: str
      character(len=:), allocatable :: trimmed_str
      if (l) then
         str = "TRUE"
      else
         str = "FALSE"
      end if
      trimmed_str = trim(str)
   end function to_string_logical

   function to_string_vector_dp(array) result(trimmed_str)
      real(kind=dp), intent(in) :: array(:)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      character(len=32) :: style
      integer :: i, total_len

      ! Set up format
      write (style, '(A,I0,A)') '(F0.', dp_precision, ')'

      ! Estimate total length needed
      total_len = 2  ! for brackets
      do i = 1, size(array)
         write (temp_str, style) array(i)
         total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string
      trimmed_str = "["
      do i = 1, size(array)
         write (temp_str, style) array(i)
         if (i < size(array)) then
            trimmed_str = trimmed_str//trim(temp_str)//", "
         else
            trimmed_str = trimmed_str//trim(temp_str)
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_vector_dp

! Vector to_string functions

   function to_string_vector_int32(array) result(trimmed_str)
      integer(int32), intent(in) :: array(:)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      integer :: i, total_len

      ! Estimate total length needed
      total_len = 2  ! for brackets
      do i = 1, size(array)
         write (temp_str, '(I0)') array(i)
         total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string
      trimmed_str = "["
      do i = 1, size(array)
         write (temp_str, '(I0)') array(i)
         if (i < size(array)) then
            trimmed_str = trimmed_str//trim(temp_str)//", "
         else
            trimmed_str = trimmed_str//trim(temp_str)
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_vector_int32

   function to_string_vector_int64(array) result(trimmed_str)
      integer(int64), intent(in) :: array(:)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      integer :: i, total_len

      ! Estimate total length needed
      total_len = 2  ! for brackets
      do i = 1, size(array)
         write (temp_str, '(I0)') array(i)
         total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string
      trimmed_str = "["
      do i = 1, size(array)
         write (temp_str, '(I0)') array(i)
         if (i < size(array)) then
            trimmed_str = trimmed_str//trim(temp_str)//", "
         else
            trimmed_str = trimmed_str//trim(temp_str)
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_vector_int64

   function to_string_vector_sp(array) result(trimmed_str)
      real(kind=sp), intent(in) :: array(:)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      character(len=32) :: style
      integer :: i, total_len

      ! Set up format
      write (style, '(A,I0,A)') '(F0.', sp_precision, ')'

      ! Estimate total length needed
      total_len = 2  ! for brackets
      do i = 1, size(array)
         write (temp_str, style) array(i)
         total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string
      trimmed_str = "["
      do i = 1, size(array)
         write (temp_str, style) array(i)
         if (i < size(array)) then
            trimmed_str = trimmed_str//trim(temp_str)//", "
         else
            trimmed_str = trimmed_str//trim(temp_str)
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_vector_sp

   function to_string_matrix_dp(array) result(trimmed_str)
      real(kind=dp), intent(in) :: array(:, :)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      character(len=32) :: style
      integer :: i, j, total_len, nrows, ncols

      nrows = size(array, 1)
      ncols = size(array, 2)

      ! Set up format
      write (style, '(A,I0,A)') '(F0.', dp_precision, ')'

      ! Estimate total length needed (rough estimate)
      total_len = 10 + nrows  ! for outer brackets and newlines
      do i = 1, nrows
         total_len = total_len + 3  ! for row brackets and comma
         do j = 1, ncols
            write (temp_str, style) array(i, j)
            total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
         end do
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string with newlines
      trimmed_str = "["//new_line('a')
      do i = 1, nrows
         trimmed_str = trimmed_str//" ["
         do j = 1, ncols
            write (temp_str, style) array(i, j)
            if (j < ncols) then
               trimmed_str = trimmed_str//trim(temp_str)//", "
            else
               trimmed_str = trimmed_str//trim(temp_str)
            end if
         end do
         if (i < nrows) then
            trimmed_str = trimmed_str//"],"//new_line('a')
         else
            trimmed_str = trimmed_str//"]"//new_line('a')
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_matrix_dp

   function to_string_matrix_int32(array) result(trimmed_str)
      integer(int32), intent(in) :: array(:, :)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      integer :: i, j, total_len, nrows, ncols

      nrows = size(array, 1)
      ncols = size(array, 2)

      ! Estimate total length needed
      total_len = 10  ! for outer brackets and newlines
      do i = 1, nrows
         total_len = total_len + 3  ! for row brackets and comma
         do j = 1, ncols
            write (temp_str, '(I0)') array(i, j)
            total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
         end do
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string
      trimmed_str = "["
      do i = 1, nrows
         if (i > 1) trimmed_str = trimmed_str//", "
         trimmed_str = trimmed_str//"["
         do j = 1, ncols
            write (temp_str, '(I0)') array(i, j)
            if (j < ncols) then
               trimmed_str = trimmed_str//trim(temp_str)//", "
            else
               trimmed_str = trimmed_str//trim(temp_str)
            end if
         end do
         trimmed_str = trimmed_str//"]"
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_matrix_int32

   function to_string_matrix_int64(array) result(trimmed_str)
      integer(int64), intent(in) :: array(:, :)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      integer :: i, j, total_len, nrows, ncols

      nrows = size(array, 1)
      ncols = size(array, 2)

      ! Estimate total length needed
      total_len = 10 + nrows  ! for outer brackets and newlines
      do i = 1, nrows
         total_len = total_len + 3  ! for row brackets and comma
         do j = 1, ncols
            write (temp_str, '(I0)') array(i, j)
            total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
         end do
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string with newlines
      trimmed_str = "["//new_line('a')
      do i = 1, nrows
         trimmed_str = trimmed_str//" ["
         do j = 1, ncols
            write (temp_str, '(I0)') array(i, j)
            if (j < ncols) then
               trimmed_str = trimmed_str//trim(temp_str)//", "
            else
               trimmed_str = trimmed_str//trim(temp_str)
            end if
         end do
         if (i < nrows) then
            trimmed_str = trimmed_str//"],"//new_line('a')
         else
            trimmed_str = trimmed_str//"]"//new_line('a')
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_matrix_int64

   function to_string_matrix_sp(array) result(trimmed_str)
      real(kind=sp), intent(in) :: array(:, :)
      character(len=:), allocatable :: trimmed_str
      character(len=50) :: temp_str
      character(len=32) :: style
      integer :: i, j, total_len, nrows, ncols

      nrows = size(array, 1)
      ncols = size(array, 2)

      ! Set up format
      write (style, '(A,I0,A)') '(F0.', sp_precision, ')'

      ! Estimate total length needed
      total_len = 10 + nrows  ! for outer brackets and newlines
      do i = 1, nrows
         total_len = total_len + 3  ! for row brackets and comma
         do j = 1, ncols
            write (temp_str, style) array(i, j)
            total_len = total_len + len_trim(temp_str) + 2  ! +2 for ", "
         end do
      end do

      ! Allocate result string
      allocate (character(len=total_len) :: trimmed_str)

      ! Build the string with newlines
      trimmed_str = "["//new_line('a')
      do i = 1, nrows
         trimmed_str = trimmed_str//" ["
         do j = 1, ncols
            write (temp_str, style) array(i, j)
            if (j < ncols) then
               trimmed_str = trimmed_str//trim(temp_str)//", "
            else
               trimmed_str = trimmed_str//trim(temp_str)
            end if
         end do
         if (i < nrows) then
            trimmed_str = trimmed_str//"],"//new_line('a')
         else
            trimmed_str = trimmed_str//"]"//new_line('a')
         end if
      end do
      trimmed_str = trimmed_str//"]"
   end function to_string_matrix_sp

end module pic_string
