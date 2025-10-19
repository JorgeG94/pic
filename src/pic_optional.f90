!! Optional value handling module
module pic_optional_value
   !! This module provides functions to handle optional arguments
   use pic_types, only: sp, dp, int32, int64
   implicit none

   private

   public :: pic_optional

   interface pic_optional
   !! Overloaded interface for optional value retrieval, supported types are:
   !! - integer(int32), integer(int64), real(sp), real(dp), character(len=*), logical
      module procedure :: optional_int32
      module procedure :: optional_int64
      module procedure :: optional_sp
      module procedure :: optional_dp
      module procedure :: optional_char
      module procedure :: optional_logical
   end interface

contains

   pure function optional_int32(input_value, default_value) result(output)
      !! Handle optional integer(int32) value
      integer(int32), intent(in), optional :: input_value
      integer(int32), intent(in) :: default_value
      integer(int32) :: output

      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if
   end function optional_int32

   pure function optional_int64(input_value, default_value) result(output)
      !! Handle optional integer(int64) value
      integer(int64), intent(in), optional :: input_value
      integer(int64), intent(in) :: default_value
      integer(int64) :: output

      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if
   end function optional_int64

   pure function optional_sp(input_value, default_value) result(output)
      !! Handle optional real(sp) value
      real(sp), intent(in), optional :: input_value
      real(sp), intent(in) :: default_value
      real(sp) :: output

      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if
   end function optional_sp

   pure function optional_dp(input_value, default_value) result(output)
      !! Handle optional real(dp) value
      real(dp), intent(in), optional :: input_value
      real(dp), intent(in) :: default_value
      real(dp) :: output

      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if
   end function optional_dp

   pure function optional_char(input_value, default_value) result(output)
      !! Handle optional character(len=*) value
      character(len=*), intent(in), optional :: input_value
      character(len=*), intent(in) :: default_value
      character(len=:), allocatable :: output
      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if

   end function optional_char

   pure function optional_logical(input_value, default_value) result(output)
      !! Handle optional logical value
      logical, intent(in), optional :: input_value
      logical, intent(in) :: default_value
      logical :: output

      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if
   end function optional_logical

end module pic_optional_value
