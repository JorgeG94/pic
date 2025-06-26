module pic_global_definitions
   use pic_types, only: default_int
   implicit none

   public
   integer(default_int), parameter :: stdout = 6
   integer(default_int), parameter :: logfile_unit = 99

end module pic_global_definitions
