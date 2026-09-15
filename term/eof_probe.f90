! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
program pic_term_eof_probe
   !! Checks that a closed standard input is reported as end of input.
   !!
   !! It points its own standard input at the null device first, which is the
   !! cheapest stream that is permanently readable and permanently empty --
   !! the same shape as a pipe whose writer has exited, and the case that
   !! matters. `poll` says "ready", `read` returns nothing, and if that is
   !! reported as a successful zero-byte read then every input loop in the
   !! module documentation turns into a spin at 100% of a core. So the one
   !! thing this asserts is that it is *not* reported that way.
   !!
   !! The redirection is done in-process rather than by the test command,
   !! because a redirect in the test command has to be written in a shell and
   !! the shell differs by platform -- see the comment in eof_probe_stdin.c.
   !! This way ctest runs the program with no shell at all.
   !!
   !! Its own negative control is the iteration count: if the reads were
   !! genuinely timing out rather than hitting end of input, the loop would
   !! exhaust its budget and the program would fail rather than pass by
   !! default.
   use, intrinsic :: iso_c_binding, only: c_int
   use pic_types, only: default_int
   use pic_error, only: error_t
   use pic_term, only: term_read
   implicit none

   interface
      subroutine stdin_from_null(status) bind(c, name="pic_term_test_stdin_from_null")
         import :: c_int
         implicit none
         integer(c_int), intent(out) :: status
      end subroutine stdin_from_null
   end interface

   integer(default_int), parameter :: MAX_READS = 100_default_int

   character(len=64) :: buf
   integer(default_int) :: n, i
   type(error_t) :: err
   logical :: at_eof
   integer(default_int) :: failures
   integer(c_int) :: redirect_status

   failures = 0_default_int

   call stdin_from_null(redirect_status)
   if (redirect_status /= 0_c_int) then
      ! Without this the rest would be meaningless rather than merely failing:
      ! reads of the inherited standard input would time out, and a timeout is
      ! exactly what this test exists to tell apart from end of input.
      write (*, "(a)") "  FAIL  could not point standard input at the null device"
      error stop 1
   end if

   at_eof = .false.
   do i = 1_default_int, MAX_READS
      call term_read(buf, n, 50_default_int, err, at_eof=at_eof)
      if (err%has_error()) then
         write (*, "(a)") "  FAIL  reading a closed stdin reported an error"
         failures = failures + 1_default_int
         exit
      end if
      if (at_eof) exit
      if (n > 0_default_int) then
         write (*, "(a)") "  FAIL  a closed stdin produced bytes"
         failures = failures + 1_default_int
         exit
      end if
   end do

   if (.not. at_eof .and. failures == 0_default_int) then
      write (*, "(a,i0,a)") "  FAIL  ", MAX_READS, &
         " reads of a closed stdin never reported end of input"
      failures = failures + 1_default_int
   else if (at_eof) then
      write (*, "(a,i0,a)") "  ok    end of input reported after ", i, " read(s)"
   end if

   ! With no `at_eof` to report it into, the same condition must surface as an
   ! error. A caller that cannot see end of input at all is the one that loops
   ! for ever, so silence is not an option here.
   call term_read(buf, n, 0_default_int, err)
   if (err%has_error()) then
      write (*, "(a)") "  ok    end of input is an error when at_eof is absent"
   else
      write (*, "(a)") "  FAIL  end of input passed silently with no at_eof"
      failures = failures + 1_default_int
   end if

   if (failures /= 0_default_int) error stop 1
   write (*, "(a)") "pic_term_eof_probe: all checks passed"
end program pic_term_eof_probe
