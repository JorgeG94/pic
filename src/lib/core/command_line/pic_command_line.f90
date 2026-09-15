! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! The pic_command_line.f90 file will contain all routines that will interact with the command line

module pic_command_line
  !! command line interaction module
   use pic_types, only: default_int
   use pic_error, only: error_t, ERROR_VALIDATION
   implicit none

   private
   public :: get_first_arg_from_command_line
contains

   function get_first_arg_from_command_line(err) result(filename)
      !! Get the first command line argument, expected to be a filename.
      !!
      !! Usage: `filename = get_first_arg_from_command_line(err)`
      !!
      !! For anything beyond one positional argument, prefer `pic_cli`, which
      !! parses options and flags, reports every failure through `error_t` and
      !! never writes or stops.
      !!
      !! ### Two behaviours, and why
      !!
      !! With `err` present, no argument is `ERROR_VALIDATION` and the result
      !! is blank. Nothing is printed and the process is not stopped, which is
      !! the only behaviour a library should have: it does not know that the
      !! program has a terminal, that English is wanted, or that there is
      !! nothing left to clean up.
      !!
      !! With `err` absent the old behaviour is kept -- a usage line on the
      !! standard output and `stop 1` -- because callers written against it
      !! rely on not continuing past this point, and silently returning a
      !! blank filename to them would turn a clean exit into a mystery. That
      !! path is deprecated; pass `err`.
      character(len=*), parameter :: USAGE = "Usage: ./my_executable <filename>"
      type(error_t), intent(inout), optional :: err
         !! When present, a missing argument is reported here instead of
         !! ending the process.
      character(len=255) :: filename
      character(len=255) :: arg
      integer(default_int) :: num_args

      filename = ""
      num_args = command_argument_count()

      if (num_args < 1) then
         if (present(err)) then
            call err%set(ERROR_VALIDATION, "pic_command_line: no argument given. "//USAGE)
            return
         end if
         write (*, "(A)") USAGE
         stop 1
      end if

      call get_command_argument(1, arg)

      filename = trim(adjustl(arg))

   end function get_first_arg_from_command_line

end module pic_command_line
