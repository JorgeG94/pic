!! timing routines in general

module pic_timers
  !! contains a simple timer module to measure and record time
   use pic_types
   use pic_string_utils
#ifdef _OPENMP
   use omp_lib
#endif
   implicit none

   type :: pic_timer
    !! derived type for a timer, contains the start, stop, and count variables
    !! can work with or without omp
      private
      real(dp) :: start, stop
      integer :: start_count, stop_count
      integer :: count_rate
   contains
      procedure :: begin => pic_timer_begin
      procedure :: finish => pic_timer_finish
      procedure :: print_time => pic_timer_print_time
      procedure :: get_elapsed_time => pic_timer_get_elapsed_time
   end type pic_timer

contains

   subroutine pic_timer_begin(self)
    !! and away we go!
      class(pic_timer), intent(inout) :: self
#ifdef _OPENMP
      self%start = omp_get_wtime()
#else
      call system_clock(self%start_count, self%count_rate)
#endif
   end subroutine pic_timer_begin

   subroutine pic_timer_finish(self)
    !! and we're done!
      class(pic_timer), intent(inout) :: self
#ifdef _OPENMP
      self%stop = omp_get_wtime()
#else
      call system_clock(self%stop_count)
#endif
   end subroutine pic_timer_finish

   subroutine pic_timer_print_time(self)
    !! print the time nicely
      class(pic_timer), intent(in) :: self
      real(dp) :: elapsed
      elapsed = self%get_elapsed_time()
      print *, "Elapsed time: "//to_string(elapsed)//" seconds"
   end subroutine pic_timer_print_time

   function pic_timer_get_elapsed_time(self) result(elapsed)
    !! return the elapsed time in double precision, in case the user wants it
      class(pic_timer), intent(in) :: self
      real(dp) :: elapsed
#ifdef _OPENMP
      elapsed = self%stop - self%start
#else
      elapsed = real(self%stop_count - self%start_count, dp)/real(self%count_rate, dp)
#endif
   end function pic_timer_get_elapsed_time

end module pic_timers
