!! flop rate handler modules
module pic_flop_rate
   !! pic_flop_rate is a convenient encapsulation of the flop_recorder and pic_timer
   !! it is used to measure the flop rate of a given operation, and report it
   use pic_types, only: dp, int64
   use pic_timer, only: pic_timer_type
   use pic_flop_recorder, only: flop_recorder_type
   use pic_string_utils, only: to_string
   implicit none
   private
   public :: flop_rate_type

   type flop_rate_type
      !! derived type for flop rate, contains a timer and a flop recorder

      private
      type(pic_timer_type) :: m_timer
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
      !! Calls the start method for the timer contained in the flop rate type
      !!
      !! Usage: call my_flop_rate%start_time()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(inout) :: self

      call self%m_timer%start()
   end subroutine flop_rate_start_time

   subroutine flop_rate_stop_time(self)
      !! Calls the stop method for the timer contained in the flop rate type
      !!
      !! Usage: call my_flop_rate%stop_time()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(inout) :: self

      call self%m_timer%stop()

   end subroutine flop_rate_stop_time

   subroutine flop_rate_add_flops(self, flops)
      !! add flops to the flop rate type, this will add the flops to the flop recorder
      !! Usage: call my_flop_rate%add_flops(1000)
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(inout) :: self
      integer(int64), intent(in) :: flops

      call self%m_flops%add(flops)

   end subroutine flop_rate_add_flops

   function flop_rate_get_flops(self) result(flops)
      !! get the number of flops recorded in the flop rate type
      !!
      !! Usage: flops = my_flop_rate%get_flops()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(in) :: self
      integer(int64) :: flops

      flops = self%m_flops%get()

   end function flop_rate_get_flops

   function flop_rate_get_time(self) result(time)
      !! get the elapsed time in seconds from the timer contained in the flop rate type
      !!
      !! Usage: time = my_flop_rate%get_time()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(in) :: self
      real(dp) :: time

      time = self%m_timer%get_elapsed_time()

   end function flop_rate_get_time

   function flop_rate_get_flop_rate(self) result(flop_rate)
      !! get the flop rate in GFLOP/s, this will calculate the flop rate based on the
      !! number of flops and the elapsed time
      !!
      !! Usage: flop_rate = my_flop_rate%get_flop_rate()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
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
      !! report the flop rate, this will print the flop rate in GFLOP/s
      !!
      !! Usage: call my_flop_rate%report()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(inout) :: self
      self%m_flop_rate = self%get_flop_rate()
      print *, "Flop rate is "//to_string(self%m_flop_rate)//" GFLOP/s"

   end subroutine flop_rate_report

   subroutine flop_rate_reset(self)
      !! reset the flop rate type, this will reset the timer and the flop recorder
      !!
      !! Usage: call my_flop_rate%reset()
      !!
      !! where my_flop_rate is an instance of flop_rate_type
      !!
      class(flop_rate_type), intent(inout) :: self

      call self%m_flops%reset()
   end subroutine flop_rate_reset

end module pic_flop_rate
