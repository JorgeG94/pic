! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
program test_pic_term
   !! Tests for pic_term, as a standalone program.
   !!
   !! Separate from `pic-tester` for two reasons. `pic_term` exists only when
   !! PIC_ENABLE_TERM is on, so registering it in the shared runner would need
   !! a preprocessor conditional in a file every other suite also edits. And
   !! these tests are about the process's own terminal state, which is not
   !! something to mix into a run of forty-odd other suites.
   !!
   !! Everything here must pass **without a terminal**, because ctest's
   !! standard input is not one. That is the interesting case anyway: a
   !! library that only behaves when attached to a tty is a library that
   !! breaks in CI, in a pipe, and under nohup.
   use pic_types, only: default_int, int64
   use pic_error, only: error_t
   use pic_clock, only: monotonic_ms
   use pic_term, only: TERM_STDIN, sleep_ms, term_is_tty, term_size, &
                       term_enable_vt, term_raw_enter, term_raw_leave, &
                       term_raw_is_active, term_read
   implicit none

   integer(default_int) :: failures

   failures = 0_default_int

   call check_not_a_tty()
   call check_raw_enter_fails_cleanly()
   call check_raw_leave_is_safe()
   call check_size_does_not_invent()
   call check_sleep_is_at_least_as_long_as_asked()
   call check_sleep_of_zero()
   call check_read_into_empty_buffer()
   call check_enable_vt()

   if (failures == 0_default_int) then
      write (*, "(a)") "test_pic_term: all checks passed"
   else
      write (*, "(a,i0,a)") "test_pic_term: ", failures, " check(s) failed"
      error stop 1
   end if

contains

   subroutine expect(condition, what)
      !! Report one check.
      logical, intent(in) :: condition
      character(len=*), intent(in) :: what

      if (condition) then
         write (*, "(a)") "  ok    "//what
      else
         write (*, "(a)") "  FAIL  "//what
         failures = failures + 1_default_int
      end if
   end subroutine expect

   subroutine check_not_a_tty()
      !! Under ctest, standard input is a pipe. A library that mistook it for
      !! a terminal would try to draw into the test log.
      call expect(.not. term_is_tty(TERM_STDIN), &
                  "term_is_tty(TERM_STDIN) is false when run under a harness")
   end subroutine check_not_a_tty

   subroutine check_raw_enter_fails_cleanly()
      !! The important word is "cleanly": an error, not an abort, and not a
      !! terminal left in a state nobody asked for.
      type(error_t) :: err

      call term_raw_enter(err)
      call expect(err%has_error(), "term_raw_enter reports an error when stdin is not a tty")
      call expect(.not. term_raw_is_active(), "and raw mode is not left active")

      ! and without err it must still not abort
      call term_raw_enter()
      call expect(.not. term_raw_is_active(), "term_raw_enter without err does not abort")
   end subroutine check_raw_enter_fails_cleanly

   subroutine check_raw_leave_is_safe()
      !! Leaving a mode that was never entered must be a no-op, so that a
      !! cleanup path can call it unconditionally.
      call term_raw_leave()
      call term_raw_leave()
      call expect(.not. term_raw_is_active(), "term_raw_leave is safe and idempotent")
   end subroutine check_raw_leave_is_safe

   subroutine check_size_does_not_invent()
      !! Either a real size, or an error. Never a plausible-looking 24x80: a
      !! caller told the size is unavailable can choose a fallback knowingly,
      !! where one handed a lie cannot.
      type(error_t) :: err
      integer(default_int) :: rows, cols

      call term_size(rows, cols, err)
      if (err%has_error()) then
         call expect(rows == 0_default_int .and. cols == 0_default_int, &
                     "term_size reports zeroes alongside its error, not a made-up size")
      else
         call expect(rows > 0_default_int .and. cols > 0_default_int, &
                     "term_size reports a positive size when it reports success")
      end if
   end subroutine check_size_does_not_invent

   subroutine check_sleep_is_at_least_as_long_as_asked()
      !! A sleep that returns early is worse than one that returns late: a
      !! loop pacing itself on it would spin.
      type(error_t) :: err
      integer(int64) :: t0, t1, elapsed

      t0 = monotonic_ms()
      call sleep_ms(50_int64, err)
      t1 = monotonic_ms()

      call expect(.not. err%has_error(), "sleep_ms(50) does not fail")
      if (t0 < 0_int64 .or. t1 < 0_int64) then
         write (*, "(a)") "  skip  no monotonic clock on this processor"
         return
      end if
      elapsed = t1 - t0
      call expect(elapsed >= 45_int64, "sleep_ms(50) does not return early")
      ! generous, because a loaded CI runner can be descheduled for a while
      call expect(elapsed < 5000_int64, "sleep_ms(50) returns within five seconds")
   end subroutine check_sleep_is_at_least_as_long_as_asked

   subroutine check_sleep_of_zero()
      type(error_t) :: err

      call sleep_ms(0_int64, err)
      call expect(.not. err%has_error(), "sleep_ms(0) returns at once and is not an error")
      call sleep_ms(-5_int64, err)
      call expect(.not. err%has_error(), "a negative sleep is not an error either")
   end subroutine check_sleep_of_zero

   subroutine check_read_into_empty_buffer()
      !! A zero-length destination must not reach the C side with a capacity
      !! it would treat as meaningful.
      type(error_t) :: err
      character(len=0) :: nothing
      integer(default_int) :: n

      call term_read(nothing, n, 0_default_int, err)
      call expect(n == 0_default_int, "reading into a zero-length buffer reads nothing")
      call expect(.not. err%has_error(), "and is not an error")
   end subroutine check_read_into_empty_buffer

   subroutine check_enable_vt()
      !! A no-op on POSIX. On Windows without a console it reports an error
      !! rather than aborting. Either way it must return.
      type(error_t) :: err

      call term_enable_vt(err)
      call expect(.true., "term_enable_vt returns rather than aborting")
   end subroutine check_enable_vt

end program test_pic_term
