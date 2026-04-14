# 1 "/home/runner/work/pic/pic/test/test_pic_profiler.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/runner/work/pic/pic/build_default//"
# 1 "/home/runner/work/pic/pic/test/test_pic_profiler.f90"
module test_pic_profiler
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_profiler
   use pic_types, only: dp
   implicit none
   private
   public :: collect_pic_profiler_tests

contains

   subroutine collect_pic_profiler_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_profiler_init_finalize", test_profiler_init_finalize), &
                  new_unittest("test_profiler_start_stop", test_profiler_start_stop), &
                  new_unittest("test_profiler_multiple_regions", test_profiler_multiple_regions), &
                  new_unittest("test_profiler_accumulated_time", test_profiler_accumulated_time), &
                  new_unittest("test_profiler_get_time", test_profiler_get_time), &
                  new_unittest("test_profiler_enable_disable", test_profiler_enable_disable), &
                  new_unittest("test_profiler_reset", test_profiler_reset), &
                  new_unittest("test_profiler_stack_based_stop", test_profiler_stack_based_stop), &
                  new_unittest("test_profiler_init_disabled", test_profiler_init_disabled), &
                  new_unittest("test_profiler_nvtx_only", test_profiler_nvtx_only), &
                  new_unittest("test_profiler_report_with_title", test_profiler_report_with_title), &
                  new_unittest("test_profiler_report_with_root", test_profiler_report_with_root), &
                  new_unittest("test_profiler_explicit_stop_nested", test_profiler_explicit_stop_nested), &
                  new_unittest("test_profiler_edge_cases", test_profiler_edge_cases) &
                  ]
   end subroutine collect_pic_profiler_tests

   subroutine test_profiler_init_finalize(error)
      !! Test profiler initialization and finalization
      type(error_type), allocatable, intent(out) :: error

      ! Initialize profiler
      call profiler_init()

      ! Finalize profiler
      call profiler_finalize()

      ! Should be able to init again after finalize
      call profiler_init()
      call profiler_finalize()

      ! Test passes if no crash
      call check(error, .true., "Profiler init/finalize should work")
   end subroutine test_profiler_init_finalize

   subroutine test_profiler_start_stop(error)
      !! Test starting and stopping a region
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      call profiler_start("test_region")
      ! Do some work
      call dummy_work()
      call profiler_stop("test_region")

      t = profiler_get_time("test_region")
      call check(error, t > 0.0_dp, "Region time should be positive")
      if (allocated(error)) then
         call profiler_finalize()
         return
      end if

      call profiler_finalize()
   end subroutine test_profiler_start_stop

   subroutine test_profiler_multiple_regions(error)
      !! Test multiple profiling regions
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t1, t2

      call profiler_init()

      call profiler_start("region_1")
      call dummy_work()
      call profiler_stop("region_1")

      call profiler_start("region_2")
      call dummy_work()
      call dummy_work()
      call profiler_stop("region_2")

      t1 = profiler_get_time("region_1")
      t2 = profiler_get_time("region_2")

      call check(error, t1 > 0.0_dp, "Region 1 time should be positive")

      call check(error, t2 > 0.0_dp, "Region 2 time should be positive")

      call profiler_finalize()
   end subroutine test_profiler_multiple_regions

   subroutine test_profiler_accumulated_time(error)
      !! Test that time accumulates across multiple start/stop pairs
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t1, t2
      integer :: i

      call profiler_init()

      ! First measurement
      call profiler_start("accumulated")
      call dummy_work()
      call profiler_stop("accumulated")
      t1 = profiler_get_time("accumulated")

      ! Additional measurements
      do i = 1, 5
         call profiler_start("accumulated")
         call dummy_work()
         call profiler_stop("accumulated")
      end do
      t2 = profiler_get_time("accumulated")

      call check(error, t2 > t1, "Time should accumulate across multiple calls")

      call profiler_finalize()
   end subroutine test_profiler_accumulated_time

   subroutine test_profiler_get_time(error)
      !! Test getting time for non-existent region
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      ! Get time for non-existent region
      t = profiler_get_time("does_not_exist")
      call check(error, t == 0.0_dp, "Non-existent region should return 0.0")

      call profiler_finalize()
   end subroutine test_profiler_get_time

   subroutine test_profiler_enable_disable(error)
      !! Test enabling and disabling profiler
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      ! Record with profiler enabled
      call profiler_start("enabled_test")
      call dummy_work()
      call profiler_stop("enabled_test")

      ! Disable profiler
      call profiler_disable()

      ! This should not be recorded
      call profiler_start("disabled_test")
      call dummy_work()
      call profiler_stop("disabled_test")

      t = profiler_get_time("disabled_test")
      call check(error, t == 0.0_dp, "Disabled region should not be recorded")

      ! Re-enable
      call profiler_enable()

      call profiler_finalize()
   end subroutine test_profiler_enable_disable

   subroutine test_profiler_reset(error)
      !! Test resetting profiler data
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      call profiler_start("reset_test")
      call dummy_work()
      call profiler_stop("reset_test")

      t = profiler_get_time("reset_test")
      call check(error, t > 0.0_dp, "Should have time before reset")

      call profiler_reset()

      t = profiler_get_time("reset_test")
      call check(error, t == 0.0_dp, "Time should be zero after reset")

      call profiler_finalize()
   end subroutine test_profiler_reset

   subroutine test_profiler_stack_based_stop(error)
      !! Test stack-based stop (no name argument)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t_outer, t_inner

      call profiler_init()

      ! Nested regions with stack-based stop
      call profiler_start("outer")
      call dummy_work()
      call profiler_start("inner")
      call dummy_work()
      call profiler_stop()  ! stops "inner"
      call dummy_work()
      call profiler_stop()  ! stops "outer"

      t_outer = profiler_get_time("outer")
      t_inner = profiler_get_time("inner")

      call check(error, t_outer > 0.0_dp, "Outer region should have time")

      call check(error, t_inner > 0.0_dp, "Inner region should have time")

      call check(error, t_outer > t_inner, "Outer should be longer than inner")

      call profiler_finalize()
   end subroutine test_profiler_stack_based_stop

   subroutine test_profiler_init_disabled(error)
      !! Test initializing profiler with enabled=.false.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init(enabled=.false.)

      ! This should not be recorded since profiler starts disabled
      call profiler_start("disabled_init_test")
      call dummy_work()
      call profiler_stop("disabled_init_test")

      t = profiler_get_time("disabled_init_test")
      call check(error, t == 0.0_dp, "Region should not be recorded when init with enabled=false")
      if (allocated(error)) then
         call profiler_finalize()
         return
      end if

      ! Enable and verify it works now
      call profiler_enable()
      call profiler_start("after_enable")
      call dummy_work()
      call profiler_stop("after_enable")

      t = profiler_get_time("after_enable")
      call check(error, t > 0.0_dp, "Region should be recorded after enable")

      call profiler_finalize()
   end subroutine test_profiler_init_disabled

   subroutine test_profiler_nvtx_only(error)
      !! Test nvtx_only regions are excluded from report but still timed
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      ! Start an nvtx_only region
      call profiler_start("nvtx_region", nvtx_only=.true.)
      call dummy_work()
      call profiler_stop("nvtx_region")

      ! Time should still be recorded
      t = profiler_get_time("nvtx_region")
      call check(error, t > 0.0_dp, "nvtx_only region should still have time")

      ! Report should not crash (nvtx_only regions excluded from print)
      call profiler_report()

      call profiler_finalize()
   end subroutine test_profiler_nvtx_only

   subroutine test_profiler_report_with_title(error)
      !! Test profiler_report with a title
      type(error_type), allocatable, intent(out) :: error

      call profiler_init()

      call profiler_start("titled_region")
      call dummy_work()
      call profiler_stop("titled_region")

      ! Should not crash
      call profiler_report(title="Test Report")

      call check(error, .true., "Report with title should not crash")
      call profiler_finalize()
   end subroutine test_profiler_report_with_title

   subroutine test_profiler_report_with_root(error)
      !! Test profiler_report with root_region for percentage calculation
      type(error_type), allocatable, intent(out) :: error

      call profiler_init()

      call profiler_start("root")
      call dummy_work()
      call profiler_start("child")
      call dummy_work()
      call profiler_stop("child")
      call profiler_stop("root")

      ! Should not crash and percentages relative to root
      call profiler_report(root_region="root")

      call check(error, .true., "Report with root_region should not crash")
      call profiler_finalize()
   end subroutine test_profiler_report_with_root

   subroutine test_profiler_explicit_stop_nested(error)
      !! Test explicit name stop with nested regions (exercises remove_from_stack)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t1, t2, t3

      call profiler_init()

      ! Start three nested regions
      call profiler_start("level1")
      call dummy_work()
      call profiler_start("level2")
      call dummy_work()
      call profiler_start("level3")
      call dummy_work()

      ! Stop middle one explicitly (not top of stack)
      call profiler_stop("level2")

      ! Stop remaining with stack-based
      call profiler_stop()  ! stops level3
      call profiler_stop()  ! stops level1

      t1 = profiler_get_time("level1")
      t2 = profiler_get_time("level2")
      t3 = profiler_get_time("level3")

      call check(error, t1 > 0.0_dp, "level1 should have time")

      call check(error, t2 > 0.0_dp, "level2 should have time")

      call check(error, t3 > 0.0_dp, "level3 should have time")

      call profiler_finalize()
   end subroutine test_profiler_explicit_stop_nested

   subroutine test_profiler_edge_cases(error)
      !! Test edge cases: stop non-existent, stop empty stack, double start
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: t

      call profiler_init()

      ! Stop non-existent region (should not crash)
      call profiler_stop("does_not_exist")

      ! Stop with empty stack (should not crash)
      call profiler_stop()

      ! Double start same region (second should be ignored)
      call profiler_start("double_start")
      call profiler_start("double_start")  ! should be no-op
      call dummy_work()
      call profiler_stop()

      t = profiler_get_time("double_start")
      call check(error, t > 0.0_dp, "Double-started region should still work")

      ! Report with no regions after finalize/init
      call profiler_finalize()
      call profiler_init()
      call profiler_report()  ! should not crash with 0 regions

      call check(error, .true., "Edge cases should not crash")
      call profiler_finalize()
   end subroutine test_profiler_edge_cases

   subroutine dummy_work()
      !! Perform some dummy computation to measure
      !! Uses cpu_time to ensure at least 1ms of work
      real(dp) :: start_cpu, end_cpu, x
      integer :: i

      call cpu_time(start_cpu)
      x = 0.0_dp
      i = 0
      do
         i = i + 1
         x = x + sin(real(i, dp))
         call cpu_time(end_cpu)
         ! Run for at least 10ms
         if (end_cpu - start_cpu > 0.01_dp) exit
         if (i > 10000000) exit  ! Safety limit
      end do
   end subroutine dummy_work

end module test_pic_profiler
