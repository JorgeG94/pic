module pic_library
   implicit none

contains

   subroutine pic_print_banner
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
      print *, ""
      print *, "========================================"
      print *, ""

   end subroutine pic_print_banner
end module pic_library
