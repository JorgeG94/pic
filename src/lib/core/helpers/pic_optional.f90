module pic_optional_value
   use pic_types, only: sp, dp, int32, int64
   implicit none

   private

   public :: pic_optional

   interface pic_optional
      module procedure :: optional_int32
      module procedure :: optional_int64
      module procedure :: optional_sp
      module procedure :: optional_dp
      module procedure :: optional_char
   end interface

contains

   pure function optional_int32(input_value, default_value) result(output)
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
      character(len=*), intent(in), optional :: input_value
      character(len=*), intent(in) :: default_value
      character(len=:), allocatable :: output
      if (present(input_value)) then
         output = input_value
      else
         output = default_value
      end if

   end function optional_char

end module pic_optional_value
