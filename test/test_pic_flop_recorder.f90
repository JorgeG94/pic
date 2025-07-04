module test_pic_flop_recorder
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_flop_recorder, only: flop_recorder_type
   use pic_types, only: int64
   implicit none
   private
   public :: collect_pic_flop_recorder_tests

contains

   subroutine collect_pic_flop_recorder_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 8
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_flop_recorder_initialization", test_flop_recorder_initialization)
      testsuite(2) = new_unittest("test_flop_recorder_add_single", test_flop_recorder_add_single)
      testsuite(3) = new_unittest("test_flop_recorder_add_multiple", test_flop_recorder_add_multiple)
      testsuite(4) = new_unittest("test_flop_recorder_reset", test_flop_recorder_reset)
      testsuite(5) = new_unittest("test_flop_recorder_large_numbers", test_flop_recorder_large_numbers)
      testsuite(6) = new_unittest("test_flop_recorder_zero_flops", test_flop_recorder_zero_flops)
      testsuite(7) = new_unittest("test_flop_recorder_accumulation", test_flop_recorder_accumulation)
      testsuite(8) = new_unittest("test_flop_recorder_multiple_instances", test_flop_recorder_multiple_instances)
   end subroutine collect_pic_flop_recorder_tests

   subroutine test_flop_recorder_initialization(error)
      !! Test that flop recorder initializes to zero
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops

      flops = recorder%get()
      call check(error, flops == 0_int64, "Flop recorder should initialize to zero")
      if (allocated(error)) return

   end subroutine test_flop_recorder_initialization

   subroutine test_flop_recorder_add_single(error)
      !! Test adding a single flop count
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops

      call recorder%add(100_int64)
      flops = recorder%get()
      call check(error, flops == 100_int64, "Should record 100 flops")
      if (allocated(error)) return

   end subroutine test_flop_recorder_add_single

   subroutine test_flop_recorder_add_multiple(error)
      !! Test adding multiple flop counts
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops

      call recorder%add(100_int64)
      call recorder%add(200_int64)
      call recorder%add(300_int64)

      flops = recorder%get()
      call check(error, flops == 600_int64, "Should accumulate to 600 flops")
      if (allocated(error)) return

   end subroutine test_flop_recorder_add_multiple

   subroutine test_flop_recorder_reset(error)
      !! Test resetting the flop counter
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops

      ! Add some flops
      call recorder%add(500_int64)
      flops = recorder%get()
      call check(error, flops == 500_int64, "Should have 500 flops before reset")
      if (allocated(error)) return

      ! Reset
      call recorder%reset()
      flops = recorder%get()
      call check(error, flops == 0_int64, "Should be zero after reset")
      if (allocated(error)) return

   end subroutine test_flop_recorder_reset

   subroutine test_flop_recorder_large_numbers(error)
      !! Test with large numbers (within int64 range)
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops
      integer(int64), parameter :: large_number = 1000000000_int64  ! 1 billion

      call recorder%add(large_number)
      call recorder%add(large_number)

      flops = recorder%get()
      call check(error, flops == 2_int64*large_number, "Should handle large numbers")
      if (allocated(error)) return

   end subroutine test_flop_recorder_large_numbers

   subroutine test_flop_recorder_zero_flops(error)
      !! Test adding zero flops
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops

      call recorder%add(100_int64)
      call recorder%add(0_int64)

      flops = recorder%get()
      call check(error, flops == 100_int64, "Adding zero should not change count")
      if (allocated(error)) return

   end subroutine test_flop_recorder_zero_flops

   subroutine test_flop_recorder_accumulation(error)
      !! Test accumulation with a loop
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder
      integer(int64) :: flops, expected_total
      integer :: i

      expected_total = 0_int64
      do i = 1, 10
         call recorder%add(int(i, int64))
         expected_total = expected_total + int(i, int64)
      end do

      flops = recorder%get()
      call check(error, flops == expected_total, "Should accumulate correctly in loop")
      if (allocated(error)) return

      ! Expected total should be 1+2+3+...+10 = 55
      call check(error, flops == 55_int64, "Should equal 55 for sum 1 to 10")
      if (allocated(error)) return

   end subroutine test_flop_recorder_accumulation

   subroutine test_flop_recorder_multiple_instances(error)
      !! Test that multiple instances are independent
      type(error_type), allocatable, intent(out) :: error
      type(flop_recorder_type) :: recorder1, recorder2
      integer(int64) :: flops1, flops2

      call recorder1%add(100_int64)
      call recorder2%add(200_int64)

      flops1 = recorder1%get()
      flops2 = recorder2%get()

      call check(error, flops1 == 100_int64, "First recorder should have 100 flops")
      if (allocated(error)) return

      call check(error, flops2 == 200_int64, "Second recorder should have 200 flops")
      if (allocated(error)) return

      ! Reset first recorder, second should be unaffected
      call recorder1%reset()
      flops1 = recorder1%get()
      flops2 = recorder2%get()

      call check(error, flops1 == 0_int64, "First recorder should be zero after reset")
      if (allocated(error)) return

      call check(error, flops2 == 200_int64, "Second recorder should be unchanged")
      if (allocated(error)) return

   end subroutine test_flop_recorder_multiple_instances

end module test_pic_flop_recorder
