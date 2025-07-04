module test_pic_flop_rate
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: dp
   use pic_flop_rate
   use pic_test_helpers, only: dummy_work
   implicit none
   private
   public :: collect_flop_rate_tests
contains

   subroutine collect_flop_rate_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 7
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_flop_rate_basic_calculation", test_flop_rate_basic_calculation)
      testsuite(2) = new_unittest("test_flop_rate_multiple_flops", test_flop_rate_multiple_flops)
      testsuite(3) = new_unittest("test_flop_rate_zero_flops", test_flop_rate_zero_flops)
      testsuite(4) = new_unittest("test_flop_rate_accessors", test_flop_rate_accessors)
      testsuite(5) = new_unittest("test_flop_rate_timing_order", test_flop_rate_timing_order)
      testsuite(6) = new_unittest("test_flop_rate_reset_behavior", test_flop_rate_reset_behavior)
      testsuite(7) = new_unittest("test_flop_rate_report", test_flop_rate_report)
   end subroutine collect_flop_rate_tests

   subroutine test_flop_rate_basic_calculation(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate
      real(dp) :: expected_rate, actual_rate
      real(dp), parameter :: tolerance = 1.0e-6_dp

      call flop_rate%start_time()
      call flop_rate%add_flops(1000_int64)
      call dummy_work()
      call flop_rate%stop_time()

      expected_rate = 1000.0_dp/flop_rate%get_time()/1.0d9
      actual_rate = flop_rate%get_flop_rate()
      print *, "expected ", expected_rate
      print *, "actual rate", actual_rate

      call check(error, abs(actual_rate - expected_rate) < tolerance)
      if (allocated(error)) return
   end subroutine test_flop_rate_basic_calculation

   subroutine test_flop_rate_multiple_flops(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate
      integer, parameter :: expected_flops = 1500

      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(500_int64)
      call flop_rate%add_flops(700_int64)
      call flop_rate%add_flops(300_int64)
      call flop_rate%stop_time()

      call check(error, flop_rate%get_flops() == expected_flops)
      if (allocated(error)) return
   end subroutine test_flop_rate_multiple_flops

   subroutine test_flop_rate_zero_flops(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate
      real(dp) :: flop_rate_value

      call flop_rate%start_time()
      call dummy_work()
      ! Add no flops
      call flop_rate%stop_time()

      call check(error, flop_rate%get_flops() == 0)
      if (allocated(error)) return

      flop_rate_value = flop_rate%get_flop_rate()
      call check(error, flop_rate_value == 0.0_dp)
      if (allocated(error)) return
   end subroutine test_flop_rate_zero_flops

   subroutine test_flop_rate_accessors(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate
      real(dp) :: time_value
      integer(int64) :: flop_count

      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(2000_int64)
      call flop_rate%stop_time()

      ! Test that accessors return reasonable values
      time_value = flop_rate%get_time()
      call check(error, time_value > 0.0_dp)
      if (allocated(error)) return

      flop_count = flop_rate%get_flops()
      call check(error, flop_count == 2000)
      if (allocated(error)) return

      call check(error, flop_rate%get_flop_rate() > 0.0_dp)
      if (allocated(error)) return

   end subroutine test_flop_rate_accessors

   subroutine test_flop_rate_timing_order(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate
      real(dp) :: time1, time2

      ! Test that timing works correctly
      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(100_int64)
      time1 = flop_rate%get_time()  ! Should be running time
      call flop_rate%stop_time()
      call dummy_work()
      call dummy_work()
      time2 = flop_rate%get_time()  ! Should be final time

      call check(error, time2 >= time1)
      if (allocated(error)) return

      call check(error, time2 > 0.0_dp)
      if (allocated(error)) return
   end subroutine test_flop_rate_timing_order

   subroutine test_flop_rate_reset_behavior(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate

      ! First measurement
      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(1000_int64)
      call flop_rate%stop_time()

      call flop_rate%reset()
      ! Second measurement should reset
      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(500_int64)
      call flop_rate%stop_time()

      ! Should only count flops from second measurement
      call check(error, flop_rate%get_flops() == 500)
      if (allocated(error)) return
   end subroutine test_flop_rate_reset_behavior

   subroutine test_flop_rate_report(error)
      type(error_type), allocatable, intent(out) :: error
      type(flop_rate_type) :: flop_rate

      call flop_rate%start_time()
      call dummy_work()
      call flop_rate%add_flops(1000_int64)
      call flop_rate%stop_time()

      ! Test that report doesn't crash (basic smoke test)
      call flop_rate%report()

      ! If we get here without crashing, the test passes
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_flop_rate_report

end module test_pic_flop_rate
