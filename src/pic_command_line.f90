!! The pic_command_line.f90 file will contain all routines that will interact with the command line

module pic_command_line
  !! command line interaction module
   use pic_types, only: default_int
   implicit none

   public

contains

   function get_first_arg_from_command_line() result(filename)
    !! obtain the first argument from the command line
      character(len=255) :: filename
      character(len=255) :: arg
      integer(default_int) :: num_args

      num_args = command_argument_count()

      if (num_args < 1) then
         write (*, '(A)') 'Usage: ./my_executable <filename>'
         stop 1
      end if

      call get_command_argument(1, arg)

      filename = trim(adjustl(arg))

   end function get_first_arg_from_command_line

end module pic_command_line
