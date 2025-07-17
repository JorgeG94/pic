!! Life is easier when we have strings. This file
!! contains the necessary routines to transform key data
!! types into strings

module pic_string_utils
!! General string utilities
   use pic_types, only: sp, dp, int32, int64
   implicit none
   ! Generic interface for to_string to handle different types
   private
   public :: to_string
   interface to_string
     !! public interface to transform variables to strings
      module procedure to_string_int32
      module procedure to_string_int64
      module procedure to_string_sp
      module procedure to_string_dp
      module procedure to_string_char
      module procedure to_string_logical
   end interface

contains

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
      !! transform a real to a string
      real(kind=sp), intent(in) :: r
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, "(F0.12)") r  ! Convert real to string with 3 decimal places
      trimmed_str = trim(str)
   end function to_string_sp

   function to_string_dp(r) result(trimmed_str)
      !! transform a real to a string
      real(kind=dp), intent(in) :: r
      character(len=50) :: str
      character(len=:), allocatable :: trimmed_str
      write (str, "(F0.12)") r  ! Convert real to string with 3 decimal places
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

end module pic_string_utils
