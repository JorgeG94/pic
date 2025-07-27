!! this is an experimental file that contains definitions
!! that will be uses across the program, for example input/output units
!! that PIC will use across things.

module pic_global_definitions
!! Global definitions for input output
   use pic_types, only: default_int, sp, dp
   implicit none

   private
   public :: stdout, logfile_unit
   public :: tol_sp, tol_dp
   integer(default_int), parameter :: stdout = 6
     !! assign output unit 6 for stdout
   integer(default_int), parameter :: logfile_unit = 99
     !! assign output unit 99 for the logfile

   real(dp), parameter :: tol_dp = 1.0e-12_dp
   real(sp), parameter :: tol_sp = 1.0e-6_sp

end module pic_global_definitions
