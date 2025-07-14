!! this file contains "helper" routines, for example printing
!! a set amount of x characters to creates tables, etc.

module pic_output_helpers
  !! Assorted output helper routines
   use pic_types, only: default_int
   implicit none
   private
   public :: print_asterisk_row

contains

   subroutine print_asterisk_row(n)
    !! prints a convenient row of asterisks of length n
      integer(kind=default_int), intent(in) :: n
      !! number of asterisks to print
      integer(kind=default_int) :: i
      do i = 1, n
         write (*, "(A)", advance="no") "*"
      end do
      write (*, *)
   end subroutine print_asterisk_row

end module pic_output_helpers
