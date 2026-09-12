! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
module test_pic_heap
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_heap, only: heap_t
   implicit none
   private

   public :: collect_pic_heap_tests

   integer(default_int), parameter :: N_RANDOM = 400

contains

   subroutine collect_pic_heap_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("init_and_state", test_init_and_state), &
                  new_unittest("single_element", test_single_element), &
                  new_unittest("two_elements", test_two_elements), &
                  new_unittest("push_pop_sorted", test_push_pop_sorted), &
                  new_unittest("payload_travels", test_payload_travels), &
                  new_unittest("growth_past_capacity", test_growth_past_capacity), &
                  new_unittest("reserve", test_reserve), &
                  new_unittest("build_from_matches_pushes", test_build_from_matches_pushes), &
                  new_unittest("build_from_edge_cases", test_build_from_edge_cases), &
                  new_unittest("empty_pop_and_peek", test_empty_pop_and_peek), &
                  new_unittest("equal_keys_are_fifo", test_equal_keys_are_fifo), &
                  new_unittest("clear_then_reuse", test_clear_then_reuse), &
                  new_unittest("destroy", test_destroy), &
                  new_unittest("max_heap", test_max_heap) &
                  ]
   end subroutine collect_pic_heap_tests

   !> Deterministic 31-bit linear congruential generator. Kept inline and
   !> local so the tests do not depend on any other module. The arithmetic is
   !> done in int64 and stays well below huge(1_int64), so no overflow.
   subroutine lcg_next(state, value)
      integer(int64), intent(inout) :: state
      integer(int64), intent(out) :: value
      state = mod(1103515245_int64*state + 12345_int64, 2147483648_int64)
      value = state
   end subroutine lcg_next

   subroutine test_init_and_state(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(heap_t) :: fresh

      call check(error, fresh%capacity() == 0, "a fresh heap owns no storage")
      if (allocated(error)) return
      call check(error, fresh%size() == 0, "a fresh heap is of size zero")
      if (allocated(error)) return
      call check(error, fresh%is_empty(), "a fresh heap is empty")
      if (allocated(error)) return

      call heap%init()
      call check(error, heap%capacity() >= 16, "default init reserves the default capacity")
      if (allocated(error)) return
      call check(error, heap%size() == 0, "init leaves the heap empty")
      if (allocated(error)) return

      call heap%init(100_default_int)
      call check(error, heap%capacity() >= 100, "init honours an explicit capacity")
      if (allocated(error)) return

      call heap%push(1_int64, 1_int32)
      call heap%init()
      call check(error, heap%is_empty(), "re-init discards the previous contents")
      if (allocated(error)) return
   end subroutine test_init_and_state

   subroutine test_single_element(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload

      call heap%init()
      call heap%push(42_int64, 7_int32)
      call check(error, heap%size() == 1, "one push gives size one")
      if (allocated(error)) return
      call check(error, .not. heap%is_empty(), "a heap with one entry is not empty")
      if (allocated(error)) return

      call heap%peek(key, payload, err)
      call check(error, .not. err%has_error(), "peek on a non-empty heap succeeds")
      if (allocated(error)) return
      call check(error, key == 42_int64 .and. payload == 7_int32, "peek returns the only entry")
      if (allocated(error)) return
      call check(error, heap%size() == 1, "peek does not remove the entry")
      if (allocated(error)) return

      call heap%pop(key, payload, err)
      call check(error, .not. err%has_error(), "pop on a non-empty heap succeeds")
      if (allocated(error)) return
      call check(error, key == 42_int64 .and. payload == 7_int32, "pop returns the only entry")
      if (allocated(error)) return
      call check(error, heap%is_empty(), "the heap is empty after the last pop")
      if (allocated(error)) return
   end subroutine test_single_element

   subroutine test_two_elements(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload

      call heap%init()
      call heap%push(9_int64, 90_int32)
      call heap%push(4_int64, 40_int32)

      call heap%pop(key, payload, err)
      call check(error, key == 4_int64 .and. payload == 40_int32, "the smaller of two entries pops first")
      if (allocated(error)) return
      call heap%pop(key, payload, err)
      call check(error, key == 9_int64 .and. payload == 90_int32, "the larger of two entries pops second")
      if (allocated(error)) return
      call check(error, heap%is_empty(), "both entries were removed")
      if (allocated(error)) return

      ! same pair, pushed in the other order
      call heap%push(4_int64, 40_int32)
      call heap%push(9_int64, 90_int32)
      call heap%pop(key, payload, err)
      call check(error, key == 4_int64, "insertion order does not change the minimum")
      if (allocated(error)) return
   end subroutine test_two_elements

   subroutine test_push_pop_sorted(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: state, value, key, previous
      integer(int32) :: payload
      integer(default_int) :: i, popped

      call heap%init()
      state = 2024_int64
      do i = 1, N_RANDOM
         call lcg_next(state, value)
         call heap%push(mod(value, 100000_int64), int(i, int32))
      end do

      call check(error, heap%size() == N_RANDOM, "every pushed entry is stored")
      if (allocated(error)) return

      previous = -1_int64
      popped = 0
      do while (.not. heap%is_empty())
         call heap%pop(key, payload, err)
         if (err%has_error()) exit
         call check(error, key >= previous, "keys pop in non-decreasing order")
         if (allocated(error)) return
         previous = key
         popped = popped + 1
      end do

      call check(error, popped == N_RANDOM, "every pushed entry pops back out")
      if (allocated(error)) return
   end subroutine test_push_pop_sorted

   subroutine test_payload_travels(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: state, value, key
      integer(int64) :: key_of(N_RANDOM)
      integer(int32) :: payload
      integer(default_int) :: i

      call heap%init(N_RANDOM)
      state = 99_int64
      do i = 1, N_RANDOM
         call lcg_next(state, value)
         key_of(i) = mod(value, 500_int64)
         call heap%push(key_of(i), int(i, int32))
      end do

      do i = 1, N_RANDOM
         call heap%pop(key, payload, err)
         call check(error, payload >= 1_int32 .and. payload <= int(N_RANDOM, int32), "payload stays in range")
         if (allocated(error)) return
         call check(error, key == key_of(int(payload, default_int)), "the payload travels with its own key")
         if (allocated(error)) return
      end do
   end subroutine test_payload_travels

   subroutine test_growth_past_capacity(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: state, value, key, previous
      integer(int32) :: payload
      integer(default_int) :: i, popped
      integer(default_int), parameter :: N_GROW = 1000

      ! start from nothing at all: no init, no reserve
      state = 7_int64
      do i = 1, N_GROW
         call lcg_next(state, value)
         call heap%push(mod(value, 7919_int64), int(i, int32))
      end do

      call check(error, heap%size() == N_GROW, "nothing is lost while the storage grows")
      if (allocated(error)) return
      call check(error, heap%capacity() >= N_GROW, "capacity grew to fit the entries")
      if (allocated(error)) return

      previous = -1_int64
      popped = 0
      do i = 1, N_GROW
         call heap%pop(key, payload, err)
         call check(error, .not. err%has_error(), "pop succeeds while entries remain")
         if (allocated(error)) return
         call check(error, key >= previous, "the heap property survived the reallocations")
         if (allocated(error)) return
         previous = key
         popped = popped + 1
      end do
      call check(error, popped == N_GROW, "all grown entries popped back out")
      if (allocated(error)) return
   end subroutine test_growth_past_capacity

   subroutine test_reserve(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload
      integer(default_int) :: big_capacity

      call heap%init(8_default_int)
      call heap%push(3_int64, 30_int32)
      call heap%push(1_int64, 10_int32)

      call heap%reserve(2_default_int)
      call check(error, heap%capacity() >= 8, "reserve never shrinks the storage")
      if (allocated(error)) return

      call heap%reserve(4096_default_int)
      big_capacity = heap%capacity()
      call check(error, big_capacity >= 4096, "reserve grows the storage on request")
      if (allocated(error)) return
      call check(error, heap%size() == 2, "reserve preserves the entry count")
      if (allocated(error)) return

      call heap%pop(key, payload, err)
      call check(error, key == 1_int64 .and. payload == 10_int32, "reserve preserves the entries themselves")
      if (allocated(error)) return
      call heap%pop(key, payload, err)
      call check(error, key == 3_int64 .and. payload == 30_int32, "reserve preserves the remaining entries")
      if (allocated(error)) return
   end subroutine test_reserve

   subroutine test_build_from_matches_pushes(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: built, pushed
      type(error_t) :: err
      integer(int64) :: state, value
      integer(int64) :: keys(N_RANDOM)
      integer(int32) :: payloads(N_RANDOM)
      integer(int64) :: key_a, key_b, previous
      integer(int32) :: payload_a, payload_b
      integer(default_int) :: i

      state = 31337_int64
      do i = 1, N_RANDOM
         call lcg_next(state, value)
         keys(i) = mod(value, 50_int64)
         payloads(i) = int(i, int32)
      end do

      call built%init()
      call built%build_from(keys, payloads, err)
      call check(error, .not. err%has_error(), "build_from on matching arrays succeeds")
      if (allocated(error)) return
      call check(error, built%size() == N_RANDOM, "build_from stores every entry")
      if (allocated(error)) return

      call pushed%init()
      do i = 1, N_RANDOM
         call pushed%push(keys(i), payloads(i))
      end do

      previous = -1_int64
      do i = 1, N_RANDOM
         call built%pop(key_a, payload_a, err)
         call pushed%pop(key_b, payload_b, err)
         call check(error, key_a >= previous, "build_from produced a valid heap")
         if (allocated(error)) return
         previous = key_a
         call check(error, key_a == key_b, "build_from pops the same keys as repeated pushes")
         if (allocated(error)) return
         call check(error, payload_a == payload_b, "build_from pops the same payloads as repeated pushes")
         if (allocated(error)) return
      end do

      call check(error, built%is_empty() .and. pushed%is_empty(), "both heaps drained")
      if (allocated(error)) return
   end subroutine test_build_from_matches_pushes

   subroutine test_build_from_edge_cases(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: keys(3), empty_keys(0)
      integer(int32) :: payloads(2), empty_payloads(0)

      keys = [5_int64, 1_int64, 3_int64]
      payloads = [50_int32, 10_int32]

      call heap%init()
      call heap%push(11_int64, 1_int32)
      call heap%build_from(keys, payloads, err)
      call check(error, err%has_error(), "mismatched array lengths are an error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "the mismatch is a validation error")
      if (allocated(error)) return
      call check(error, heap%size() == 1, "a rejected build_from leaves the heap untouched")
      if (allocated(error)) return

      ! same rejection, but without asking for the error back: must not crash
      call heap%build_from(keys, payloads)
      call check(error, heap%size() == 1, "a rejected build_from without err leaves the heap untouched")
      if (allocated(error)) return

      ! zero-length build on a heap that owns no storage at all
      block
         type(heap_t) :: virgin
         call virgin%build_from(empty_keys, empty_payloads, err)
         call check(error, .not. err%has_error(), "an empty build_from is not an error")
         if (allocated(error)) return
         call check(error, virgin%is_empty(), "an empty build_from yields an empty heap")
         if (allocated(error)) return
      end block

      ! build_from on a heap that already owns storage replaces the contents
      call heap%build_from(keys, [50_int32, 10_int32, 30_int32], err)
      call check(error, heap%size() == 3, "build_from replaces the previous contents")
      if (allocated(error)) return
   end subroutine test_build_from_edge_cases

   subroutine test_empty_pop_and_peek(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload

      call heap%init()

      call heap%pop(key, payload, err)
      call check(error, err%has_error(), "popping an empty heap reports an error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "the empty pop is a validation error")
      if (allocated(error)) return
      call check(error, key == 0_int64 .and. payload == 0_int32, "an empty pop zeroes its outputs")
      if (allocated(error)) return

      call err%clear()
      call heap%peek(key, payload, err)
      call check(error, err%has_error(), "peeking an empty heap reports an error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "the empty peek is a validation error")
      if (allocated(error)) return

      ! the error argument is optional: neither call may crash without it
      call heap%pop(key, payload)
      call check(error, key == 0_int64, "an unchecked empty pop still zeroes its outputs")
      if (allocated(error)) return
      call heap%peek(key, payload)
      call check(error, payload == 0_int32, "an unchecked empty peek still zeroes its outputs")
      if (allocated(error)) return

      ! and on a heap that was never initialised either
      block
         type(heap_t) :: virgin
         call virgin%pop(key, payload, err)
         call check(error, err%is(ERROR_VALIDATION), "popping an uninitialised heap is a validation error")
         if (allocated(error)) return
      end block
   end subroutine test_empty_pop_and_peek

   subroutine test_equal_keys_are_fifo(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload
      integer(default_int) :: i
      integer(int32) :: seen(6)

      ! Documented behaviour: equal keys pop in insertion (FIFO) order.
      call heap%init()
      call heap%push(5_int64, 1_int32)
      call heap%push(5_int64, 2_int32)
      call heap%push(3_int64, 100_int32)
      call heap%push(5_int64, 3_int32)
      call heap%push(5_int64, 4_int32)
      call heap%push(5_int64, 5_int32)

      call heap%pop(key, payload, err)
      call check(error, key == 3_int64 .and. payload == 100_int32, "the strictly smaller key still wins")
      if (allocated(error)) return

      do i = 1, 5
         call heap%pop(key, payload, err)
         seen(i) = payload
         call check(error, key == 5_int64, "the remaining entries all share the tied key")
         if (allocated(error)) return
      end do

      do i = 1, 5
         call check(error, seen(i) == int(i, int32), "tied keys pop in first-in-first-out order")
         if (allocated(error)) return
      end do

      ! build_from breaks ties in array order, matching pushes in that order
      call heap%build_from([7_int64, 7_int64, 7_int64], [11_int32, 22_int32, 33_int32], err)
      do i = 1, 3
         call heap%pop(key, payload, err)
         seen(i) = payload
      end do
      call check(error, seen(1) == 11_int32 .and. seen(2) == 22_int32 .and. seen(3) == 33_int32, &
                 "build_from breaks ties in array order")
      if (allocated(error)) return
   end subroutine test_equal_keys_are_fifo

   subroutine test_clear_then_reuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload
      integer(default_int) :: i, kept_capacity

      call heap%init()
      do i = 1, 50
         call heap%push(int(51 - i, int64), int(i, int32))
      end do
      kept_capacity = heap%capacity()

      call heap%clear()
      call check(error, heap%is_empty(), "clear empties the heap")
      if (allocated(error)) return
      call check(error, heap%size() == 0, "clear resets the size")
      if (allocated(error)) return
      call check(error, heap%capacity() == kept_capacity, "clear keeps the storage for reuse")
      if (allocated(error)) return

      call heap%pop(key, payload, err)
      call check(error, err%has_error(), "a cleared heap pops as empty")
      if (allocated(error)) return

      call heap%push(2_int64, 20_int32)
      call heap%push(2_int64, 21_int32)
      call heap%push(1_int64, 10_int32)
      call heap%pop(key, payload, err)
      call check(error, key == 1_int64 .and. payload == 10_int32, "a cleared heap can be reused")
      if (allocated(error)) return
      call heap%pop(key, payload, err)
      call check(error, payload == 20_int32, "the tie counter restarts cleanly after clear")
      if (allocated(error)) return
   end subroutine test_clear_then_reuse

   subroutine test_destroy(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: key
      integer(int32) :: payload

      call heap%init(64_default_int)
      call heap%push(1_int64, 1_int32)

      call heap%destroy()
      call check(error, heap%capacity() == 0, "destroy releases the storage")
      if (allocated(error)) return
      call check(error, heap%is_empty(), "destroy empties the heap")
      if (allocated(error)) return

      ! destroying twice must be harmless
      call heap%destroy()
      call check(error, heap%capacity() == 0, "destroy is idempotent")
      if (allocated(error)) return

      ! and the heap is still usable afterwards
      call heap%push(8_int64, 80_int32)
      call heap%pop(key, payload, err)
      call check(error, key == 8_int64 .and. payload == 80_int32, "a destroyed heap can be used again")
      if (allocated(error)) return
   end subroutine test_destroy

   subroutine test_max_heap(error)
      type(error_type), allocatable, intent(out) :: error
      type(heap_t) :: heap
      type(error_t) :: err
      integer(int64) :: state, value, key, previous
      integer(int32) :: payload
      integer(default_int) :: i

      call heap%init(max_heap=.true.)
      state = 5150_int64
      do i = 1, N_RANDOM
         call lcg_next(state, value)
         call heap%push(mod(value, 1000_int64), int(i, int32))
      end do

      call heap%peek(key, payload, err)
      call check(error, .not. err%has_error(), "peek works on a max-heap")
      if (allocated(error)) return

      previous = huge(1_int64)
      do i = 1, N_RANDOM
         call heap%pop(key, payload, err)
         call check(error, key <= previous, "a max-heap pops keys in non-increasing order")
         if (allocated(error)) return
         previous = key
      end do
      call check(error, heap%is_empty(), "the max-heap drained")
      if (allocated(error)) return

      ! ties stay FIFO in a max-heap too
      call heap%init(max_heap=.true.)
      call heap%push(4_int64, 41_int32)
      call heap%push(4_int64, 42_int32)
      call heap%push(9_int64, 90_int32)
      call heap%pop(key, payload, err)
      call check(error, key == 9_int64, "the larger key pops first from a max-heap")
      if (allocated(error)) return
      call heap%pop(key, payload, err)
      call check(error, payload == 41_int32, "a max-heap breaks ties first-in-first-out")
      if (allocated(error)) return
      call heap%pop(key, payload, err)
      call check(error, payload == 42_int32, "the second tied max-heap entry follows")
      if (allocated(error)) return
   end subroutine test_max_heap

end module test_pic_heap
