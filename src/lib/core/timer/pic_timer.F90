!! timing routines in general

module pic_timer
  !! contains a simple timer module to measure and record time
   use pic_types, only: dp, default_int
   use pic_string, only: to_string
#ifdef _OPENMP
   use omp_lib, only: omp_get_wtime
#endif
   implicit none
   private
   public :: pic_timer_type

   type :: pic_timer_type
    !! derived type for a timer, contains the start, stop, and count variables
    !! can work with or without omp. If PIC is compiled with OpenMP the default
    !! timer will be the omp time. This is mostly to minimize problems with threading
    !! and system clock
      private
      real(dp) :: start_time = 0.0_dp
      real(dp) :: stop_time = 0.0_dp
      real(dp) :: walltime
      logical :: is_running = .false.
      integer(default_int) :: start_count = 0_default_int
      integer(default_int) :: stop_count = 0_default_int
      integer(default_int) :: count_rate = 1_default_int
   contains
      procedure, non_overridable :: start => timer_start
      procedure, non_overridable :: stop => timer_stop
      procedure, non_overridable :: print_time => timer_print_time
      procedure, non_overridable :: get_elapsed_time => timer_get_elapsed_time
   end type pic_timer_type

contains

   subroutine timer_start(self)
      !! starts the timer. If OMP is enabled, it will use omp_get_wtime()
      !! if not, it will use Fortran's system_clock
      !!
      !! Usage: call my_timer%start()
      !!
      !! Usage assumes a declaration of type(pic_timer_type) :: my_timer
      class(pic_timer_type), intent(inout) :: self
      self%is_running = .true.
#ifdef _OPENMP
      self%start_time = omp_get_wtime()
#else
      call system_clock(self%start_count, self%count_rate)
#endif
   end subroutine timer_start

   subroutine timer_stop(self)
      !! stop the timer. If OMP is enabled, it will use omp_get_wtime()
      !! if not, it will use Fortran's system_clock
      !!
      !! Usage: call my_timer%stop()
      !!
      !! Usage assumes a declaration of type(pic_timer_type) :: my_timer
      !! will fail if a timer has not been started!
      class(pic_timer_type), intent(inout) :: self
      if (.not. self%is_running) then
         error stop "Cannot stop a timer that has not been started!"
      end if
#ifdef _OPENMP
      self%stop_time = omp_get_wtime()
#else
      call system_clock(self%stop_count)
#endif
      ! if someone stops the timer, we stop !
      self%is_running = .false.
   end subroutine timer_stop

   subroutine timer_print_time(self)
      !! Prints the elapsed time at the time of calling
      !!
      !! Usage: call my_timer%print_time()
      !!
      !! Needs my_timer to be declared previously as type(pic_timer_type) :: my_timer
      !!
      !! This function does not stop the timer, it will get the current time elapsed stopped or not
      class(pic_timer_type), intent(in) :: self
      real(dp) :: elapsed

      elapsed = self%get_elapsed_time()
      if (self%is_running) then
         print *, "Currently elapsed time: "//to_string(elapsed)//" seconds"
      else
         print *, "Elapsed time: "//to_string(elapsed)//" seconds"
      end if
   end subroutine timer_print_time

   function timer_get_elapsed_time(self) result(elapsed)
      !! Returns the elapsed time as a real(dp) variable
      !!
      !! Usage: var = my_timer%get_elapsed_time()
      !!
      !! Needs my_timer to be declared previously as type(pic_timer_type) :: my_timer
      !!
      class(pic_timer_type), intent(in) :: self
      real(dp) :: elapsed
      integer(default_int) :: current_count
      elapsed = 0.0_dp
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

end module pic_timer
