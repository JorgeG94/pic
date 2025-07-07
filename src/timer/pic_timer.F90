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
      real(dp) :: start_time, stop_time
      real(dp) :: walltime
      logical :: is_running = .false.
      integer :: start_count, stop_count
      integer :: count_rate
   contains
      procedure :: start => timer_start
      procedure :: stop => timer_stop
      procedure :: print_time => timer_print_time
      procedure :: get_elapsed_time => timer_get_elapsed_time
      procedure :: set
   end type pic_timer

contains

   subroutine timer_start(self)
    !! and away we go!
      class(pic_timer), intent(inout) :: self
      self%is_running = .true.
#ifdef _OPENMP
      self%start_time = omp_get_wtime()
#else
      call system_clock(self%start_count, self%count_rate)
#endif
   end subroutine timer_start

   subroutine timer_stop(self)
    !! and we're done!
      class(pic_timer), intent(inout) :: self
#ifdef _OPENMP
      self%stop_time = omp_get_wtime()
#else
      call system_clock(self%stop_count)
#endif
      ! if someone stops the timer, we stop !
      self%is_running = .false.
   end subroutine timer_stop

   subroutine timer_print_time(self)
    !! print the time nicely
      class(pic_timer), intent(in) :: self
      real(dp) :: elapsed
      elapsed = self%get_elapsed_time()
      print *, "Elapsed time: "//to_string(elapsed)//" seconds"
   end subroutine timer_print_time

   function timer_get_elapsed_time(self) result(elapsed)
      class(pic_timer), intent(in) :: self
      real(dp) :: elapsed
      integer :: current_count
#ifdef _OPENMP
      if (self%is_running) then
         elapsed = omp_get_wtime() - self%start_time
      else
         elapsed = self%stop_time - self%start_time
      end if
#else
      if (self%is_running) then
         call system_clock(count=current_count)
         elapsed = real(current_count - self%start_count, dp)/real(self%count_rate, dp)
      else
         elapsed = real(self%stop_count - self%start_count, dp)/real(self%count_rate, dp)
      end if
#endif
   end function timer_get_elapsed_time

   subroutine set(self, time)
      class(pic_timer), intent(inout) :: self
      real(dp), intent(in) :: time
      self%walltime = time
   end subroutine set

end module pic_timers
