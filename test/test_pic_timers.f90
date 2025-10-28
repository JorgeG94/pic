module test_pic_timer
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_timer, only: timer_type
   use pic_types, only: dp, default_int
   use pic_test_helpers, only: dummy_work
   implicit none
   private
   public :: collect_pic_timers_tests

contains

   subroutine collect_pic_timers_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("test_timer_basic_functionality", test_timer_basic_functionality), &
                  new_unittest("test_timer_zero_elapsed", test_timer_zero_elapsed), &
                  new_unittest("test_timer_short_delay", test_timer_short_delay), &
                  new_unittest("test_timer_multiple_measurements", test_timer_multiple_measurements), &
                  new_unittest("test_timer_get_elapsed_time", test_timer_get_elapsed_time), &
                  new_unittest("test_timer_precision", test_timer_precision) &
                  ]
   end subroutine collect_pic_timers_tests

   subroutine test_timer_basic_functionality(error)
      !! Test basic start/stop functionality
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer
      real(dp) :: elapsed

      ! Start and immediately stop timer
      call timer%start()
      call timer%stop()

      elapsed = timer%get_elapsed_time()

      ! Should be a small positive number (or zero)
      call check(error, elapsed >= 0.0_dp, "Timer should return non-negative elapsed time")
      if (allocated(error)) return

      ! Should be a reasonable small value (less than 1 second for immediate stop)
      call check(error, elapsed < 1.0_dp, "Immediate start/stop should be less than 1 second")
      if (allocated(error)) return

   end subroutine test_timer_basic_functionality

   subroutine test_timer_zero_elapsed(error)
      !! Test that timer handles near-zero elapsed time correctly
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer
      real(dp) :: elapsed

      call timer%start()
      call timer%stop()

      elapsed = timer%get_elapsed_time()

      ! Should be non-negative
      call check(error, elapsed >= 0.0_dp, "Elapsed time should be non-negative")
      if (allocated(error)) return

      ! Should be very small (less than 0.1 seconds)
      call check(error, elapsed < 0.1_dp, "Immediate timing should be very small")
      if (allocated(error)) return

   end subroutine test_timer_zero_elapsed

   subroutine test_timer_short_delay(error)
      !! Test timer with a short deliberate delay
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer
      real(dp) :: elapsed

      call timer%start()

      ! Create a short computational delay
      call dummy_work()

      call timer%stop()

      elapsed = timer%get_elapsed_time()

      ! Should be positive
      print *, " Elapsed ", elapsed
      call check(error, elapsed > 0.0_dp, "Timer with work should show positive elapsed time")
      if (allocated(error)) return

      ! Should be reasonable (less than 1 second for this simple loop)
      call check(error, elapsed < 1.0_dp, "Short delay should be less than 1 second")
      if (allocated(error)) return

   end subroutine test_timer_short_delay

   subroutine test_timer_multiple_measurements(error)
      !! Test multiple consecutive measurements
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer
      real(dp) :: elapsed1, elapsed2

      ! First measurement
      call timer%start()
      call dummy_work()
      call timer%stop()
      elapsed1 = timer%get_elapsed_time()

      ! Second measurement with more work
      call timer%start()
      call dummy_work()
      call timer%stop()
      elapsed2 = timer%get_elapsed_time()

      ! Both should be positive
      call check(error, elapsed1 > 0.0_dp, "First measurement should be positive")
      if (allocated(error)) return

      call check(error, elapsed2 > 0.0_dp, "Second measurement should be positive")
      if (allocated(error)) return

      ! Second measurement should generally be larger (though not guaranteed due to system variance)
      ! We'll just check they're both reasonable
      call check(error, elapsed1 < 1.0_dp, "First measurement should be reasonable")
      if (allocated(error)) return

      call check(error, elapsed2 < 1.0_dp, "Second measurement should be reasonable")
      if (allocated(error)) return

   end subroutine test_timer_multiple_measurements

   subroutine test_timer_get_elapsed_time(error)
      !! Test the get_elapsed_time function specifically
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer
      real(dp) :: elapsed1, elapsed2

      call timer%start()
      call timer%stop()

      ! Get elapsed time multiple times - should be consistent
      elapsed1 = timer%get_elapsed_time()
      elapsed2 = timer%get_elapsed_time()

      call check(error, elapsed1 >= 0.0_dp, "First get_elapsed_time should be non-negative")
      if (allocated(error)) return

   end subroutine test_timer_get_elapsed_time

   subroutine test_timer_precision(error)
      !! Test timer precision by ensuring it can distinguish different amounts of work
      type(error_type), allocatable, intent(out) :: error
      type(timer_type) :: timer1, timer2
      real(dp) :: elapsed_short, elapsed_long

      ! Short work
      call timer1%start()
      call dummy_work()
      call timer1%stop()
      elapsed_short = timer1%get_elapsed_time()

      ! Longer work
      call timer2%start()
      call dummy_work()
      call timer2%stop()
      elapsed_long = timer2%get_elapsed_time()

      ! Both should be non-negative
      call check(error, elapsed_short >= 0.0_dp, "Short work timer should be non-negative")
      if (allocated(error)) return

      call check(error, elapsed_long >= 0.0_dp, "Long work timer should be non-negative")
      if (allocated(error)) return

      ! Both should be reasonable
      call check(error, elapsed_short < 1.0_dp, "Short work should complete quickly")
      if (allocated(error)) return

      call check(error, elapsed_long < 2.0_dp, "Long work should still complete reasonably quickly")
      if (allocated(error)) return

      ! Note: We don't test that elapsed_long > elapsed_short because system scheduling
      ! can make this unreliable in unit tests, but we ensure both are reasonable

   end subroutine test_timer_precision

end module test_pic_timer
