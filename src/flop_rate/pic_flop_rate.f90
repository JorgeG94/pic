!! flop rate handler modules
module pic_flop_rate
  !! pic_flop_rate is a convenient encapsulation of the flop_recorder and pic_timer
  !! it is used to measure the flop rate of a given operation, and report it
   use pic_types, only: dp, int64
   use pic_timers, only: pic_timer
   use pic_flop_recorder, only: flop_recorder_type
   use pic_string_utils, only: to_string
   implicit none
   private
   public :: flop_rate_type

   type flop_rate_type
  !! derived type for flop rate, contains a timer and a flop recorder

      private
      type(pic_timer) :: m_timer
      type(flop_recorder_type) :: m_flops

      real(dp) :: m_flop_rate
    !! private by default so that people use the accessor functions
   contains

      procedure :: start_time => flop_rate_start_time
      procedure :: stop_time => flop_rate_stop_time
      procedure :: add_flops => flop_rate_add_flops
      procedure :: get_flops => flop_rate_get_flops
      procedure :: get_time => flop_rate_get_time
      procedure :: get_flop_rate => flop_rate_get_flop_rate
      procedure :: report => flop_rate_report
      procedure :: reset => flop_rate_reset

   end type flop_rate_type

contains

   subroutine flop_rate_start_time(self)
  !! start the timer for the flop rate
      class(flop_rate_type), intent(inout) :: self

      call self%m_timer%start()
   end subroutine flop_rate_start_time

   subroutine flop_rate_stop_time(self)
  !! stop the timer for the flop rate
      class(flop_rate_type), intent(inout) :: self

      call self%m_timer%stop()

   end subroutine flop_rate_stop_time

   subroutine flop_rate_add_flops(self, flops)
  !! add flops to the flop rate
      class(flop_rate_type), intent(inout) :: self
      integer(int64), intent(in) :: flops

      call self%m_flops%add(flops)

   end subroutine flop_rate_add_flops

   function flop_rate_get_flops(self) result(flops)
  !! get the number of flops recorded
      class(flop_rate_type), intent(in) :: self
      integer(int64) :: flops

      flops = self%m_flops%get()

   end function flop_rate_get_flops

   function flop_rate_get_time(self) result(time)
  !! get the elapsed time for the timer through the flop rate type
      class(flop_rate_type), intent(in) :: self
      real(dp) :: time

      time = self%m_timer%get_elapsed_time()

   end function flop_rate_get_time

   function flop_rate_get_flop_rate(self) result(flop_rate)
  !! get the flop rate in GFLOP/s, return 0.0 if time is zero or negative
      class(flop_rate_type), intent(inout) :: self
      real(dp) :: flop_rate
      real(dp) :: time
      integer(int64) :: flops

      flops = self%m_flops%get()
      time = self%m_timer%get_elapsed_time()
      if (time <= 0.0_dp) then
         print *, "Warning: Time is zero or negative, setting flop rate to zero."
         self%m_flop_rate = 0.0_dp
         flop_rate = 0.0_dp
         return
      else
         self%m_flop_rate = flops/time/1.0e9_dp
         flop_rate = self%m_flop_rate
      end if
   end function flop_rate_get_flop_rate

   subroutine flop_rate_report(self)
  !! report the flop rate in GFLOP/s
  !! this is a convenience function to print the flop rate
      class(flop_rate_type), intent(inout) :: self
      self%m_flop_rate = self%get_flop_rate()
      print *, "Flop rate is "//to_string(self%m_flop_rate)//" GFLOP/s"

   end subroutine flop_rate_report

   subroutine flop_rate_reset(self)
      !! reset the flop rate, this will reset the flops, this is mostly for testing
      class(flop_rate_type), intent(inout) :: self

      call self%m_flops%reset()
   end subroutine flop_rate_reset

end module pic_flop_rate
