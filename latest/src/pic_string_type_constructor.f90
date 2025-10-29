! SPDX-License-Identifier: MIT
! Integrated from the Fortran Standard Library licensed under MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! contains the definitions for building strings from different data types

submodule(pic_string_type) pic_string_type_constructor
   use pic_strings, only: to_string
   implicit none

contains

   !> Constructor for new string instances from a scalar character value.
   elemental module function new_string(string) result(new)
      character(len=*), intent(in), optional :: string
      type(string_type) :: new
      if (present(string)) then
         new%raw = string
      end if
   end function new_string

   !> Constructor for new string instances from an integer of kind int32.
   elemental module function new_string_from_integer_int32(val) result(new)
      integer(int32), intent(in) :: val
      type(string_type) :: new
      new%raw = to_string(val)
   end function new_string_from_integer_int32
   !> Constructor for new string instances from an integer of kind int64.
   elemental module function new_string_from_integer_int64(val) result(new)
      integer(int64), intent(in) :: val
      type(string_type) :: new
      new%raw = to_string(val)
   end function new_string_from_integer_int64

   !> Constructor for new string instances from a logical of kind fbool.
   elemental module function new_string_from_logical_fbool(val) result(new)
      logical(fbool), intent(in) :: val
      type(string_type) :: new
      new%raw = to_string(val)
   end function new_string_from_logical_fbool

end submodule pic_string_type_constructor
