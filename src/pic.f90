!! the main pic module interface: versions, banners, random quotes etc will go here
module pic
   use pic_types
   use pic_debugging_tools
   use pic_mpi
   use pic_timers
   use pic_flop_recorder
   use pic_flop_rate
   use pic_logger
   use pic_global_definitions
   use pic_command_line
 !! simple interface module that prints banner and other information about the library
 !! mostly here to verify installs, etc.
   implicit none

contains

   subroutine pic_print_banner
    !! my cool banner, work in progress
      implicit none

      print *, "========================================"
      print *, "         _____  _____  _____ "
      print *, "        |  __ \\|_   _|/ ____|"
      print *, "        | |__) | | | | |     "
      print *, "        |  ___/  | | | |     "
      print *, "        | |     _| |_| |____ "
      print *, "        |_|    |_____|\\_____|"
      print *, "                                        "
      print *, "               PIC LIBRARY"
      print *, "========================================"

   end subroutine pic_print_banner
end module pic
