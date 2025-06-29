module pic_global_definitions
!! Global definitions for input output
   use pic_types, only: default_int
   implicit none

   public
   integer(default_int), parameter :: stdout = 6
     !! assign output unit 6 for stdout
   integer(default_int), parameter :: logfile_unit = 99
     !! assign output unit 99 for the logfile

end module pic_global_definitions
