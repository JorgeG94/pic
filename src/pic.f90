!! the main pic module interface: versions, banners, random quotes etc will go here
module pic
 !! simple interface module that prints banner and other information about the library
 !! mostly here to verify installs, etc.
   implicit none
   private
   public :: pic_print_banner

contains

   subroutine pic_print_banner
    !! my cool banner, work in progress

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
