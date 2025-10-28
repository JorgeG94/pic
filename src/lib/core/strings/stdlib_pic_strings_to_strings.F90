
submodule(pic_stdlib_strings) pic_stdlib_strings_to_string
   use pic_stdlib_string_type, only: string_type
   implicit none

   integer, parameter :: buffer_len = 128
   character(len=*), parameter :: err_sym = "[*]"
        !!TODO: [*]?

contains

   !> Format or transfer a real(sp) scalar as a string.
   pure module function to_string_r_sp(value, format) result(string)
      real(sp), intent(in) :: value
      character(len=*), intent(in), optional :: format
      character(len=:), allocatable :: string

      character(len=buffer_len) :: buffer
      integer :: stat

      write (buffer, '('//pic_optional(format, "g0")//')', iostat=stat) value
      if (stat == 0) then
         string = trim(buffer)
      else
         string = err_sym
      end if

   end function to_string_r_sp
   !> Format or transfer a real(dp) scalar as a string.
   pure module function to_string_r_dp(value, format) result(string)
      real(dp), intent(in) :: value
      character(len=*), intent(in), optional :: format
      character(len=:), allocatable :: string

      character(len=buffer_len) :: buffer
      integer :: stat

      write (buffer, '('//pic_optional(format, "g0")//')', iostat=stat) value
      if (stat == 0) then
         string = trim(buffer)
      else
         string = err_sym
      end if

   end function to_string_r_dp

   !> Represent an integer of kind int32 as character sequence.
   pure module function to_string_1_i_int32(value) result(string)
      integer, parameter :: ik = int32
      integer(ik), intent(in) :: value
      character(len=:), allocatable :: string
      integer, parameter :: buffer_len = range(value) + 2
      character(len=buffer_len) :: buffer
      integer :: pos
      integer(ik) :: n
      character(len=1), parameter :: numbers(-9:0) = &
                                     ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

      if (value == 0_ik) then
         string = numbers(0)
         return
      end if

      n = sign(value, -1_ik)
      buffer = ""
      pos = buffer_len + 1
      do while (n < 0_ik)
         pos = pos - 1
         buffer(pos:pos) = numbers(mod(n, 10_ik))
         n = n/10_ik
      end do

      if (value < 0_ik) then
         pos = pos - 1
         buffer(pos:pos) = '-'
      end if

      string = buffer(pos:)
   end function to_string_1_i_int32

   pure module function to_string_2_i_int32(value, format) result(string)
      integer(int32), intent(in) :: value
      character(len=*), intent(in) :: format
      character(len=:), allocatable :: string
      character(len=:), allocatable :: adjusted_format

      character(len=buffer_len) :: buffer
      integer :: stat

#ifdef __NVCOMPILER_LLVM__
      adjusted_format = fix_nvhpc_octal_format(format)
#else
      adjusted_format = format
#endif
      write (buffer, "("//adjusted_format//")", iostat=stat) value
      if (stat == 0) then
         string = trim(buffer)
      else
         string = err_sym
      end if

   end function to_string_2_i_int32
   !> Represent an integer of kind int64 as character sequence.
   pure module function to_string_1_i_int64(value) result(string)
      integer, parameter :: ik = int64
      integer(ik), intent(in) :: value
      character(len=:), allocatable :: string
      integer, parameter :: buffer_len = range(value) + 2
      character(len=buffer_len) :: buffer
      integer :: pos
      integer(ik) :: n
      character(len=1), parameter :: numbers(-9:0) = &
                                     ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

      if (value == 0_ik) then
         string = numbers(0)
         return
      end if

      n = sign(value, -1_ik)
      buffer = ""
      pos = buffer_len + 1
      do while (n < 0_ik)
         pos = pos - 1
         buffer(pos:pos) = numbers(mod(n, 10_ik))
         n = n/10_ik
      end do

      if (value < 0_ik) then
         pos = pos - 1
         buffer(pos:pos) = '-'
      end if

      string = buffer(pos:)
   end function to_string_1_i_int64

   pure module function to_string_2_i_int64(value, format) result(string)
      integer(int64), intent(in) :: value
      character(len=*), intent(in) :: format
      character(len=:), allocatable :: string
      character(len=:), allocatable :: adjusted_format

      character(len=buffer_len) :: buffer
      integer :: stat

#ifndef __NVCOMPILER_LLVM__
      adjusted_format = fix_nvhpc_octal_format(format)
#else
      adjusted_format = format
#endif
      write (buffer, "("//format//")", iostat=stat) value
      if (stat == 0) then
         string = trim(buffer)
      else
         string = err_sym
      end if

   end function to_string_2_i_int64
   pure function fix_nvhpc_octal_format(fmt) result(fixed)
      character(len=*), intent(in) :: fmt
      character(len=:), allocatable :: fixed
      integer :: pos, dot_pos
      character(len=10) :: precision_str

      ! Check if format contains "O0."
      pos = index(fmt, 'O0.')
      if (pos > 0) then
         ! Extract precision after the dot
         dot_pos = pos + 2  ! Position of '.'
         precision_str = fmt(dot_pos + 1:)
         ! Replace O0.w with Ow.w (where w is the precision)
         fixed = fmt(1:pos - 1)//'O'//trim(precision_str)//'.'//trim(precision_str)
      else
         fixed = fmt
      end if
   end function fix_nvhpc_octal_format

   !> Represent an logical of kind fbool as character sequence.
   pure module function to_string_1_l_fbool(value) result(string)
      logical(fbool), intent(in) :: value
      character(len=1) :: string

      string = merge("T", "F", value)

   end function to_string_1_l_fbool

   pure module function to_string_2_l_fbool(value, format) result(string)
      logical(fbool), intent(in) :: value
      character(len=*), intent(in) :: format
      character(len=:), allocatable :: string

      character(len=buffer_len) :: buffer
      integer :: stat

      write (buffer, "("//format//")", iostat=stat) value
      if (stat == 0) then
         string = trim(buffer)
      else
         string = err_sym
      end if

   end function to_string_2_l_fbool

end submodule pic_stdlib_strings_to_string
