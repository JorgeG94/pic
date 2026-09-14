! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! The part of a terminal interface that has to talk to the operating system.
module pic_term
   !! Raw mode, terminal size, timed reads and sleeping.
   !!
   !! This module is `.f90`, not `.F90`, and that is the design: there is not
   !! one preprocessor conditional in it, and it is byte-for-byte the same
   !! source on Linux, macOS and Windows. Every operating system difference
   !! lives in `pic_term_os.c`.
   !!
   !! The reasoning is a counting argument. A `#ifdef` in Fortran has to be
   !! right for each of the six compilers pic supports and each of three
   !! operating systems; a `#ifdef` in C has to be right for three operating
   !! systems. Pushing the conditionals across the language boundary makes the
   !! matrix smaller, and it puts them where the platform documentation is
   !! actually written.
   !!
   !! ### What crosses the boundary
   !!
   !! Only `int`, `int64_t` and `char` with an explicit length. No struct. In
   !! particular `struct termios` and `struct winsize` never appear here,
   !! because their layouts differ between Linux, macOS and the BSDs and a
   !! Fortran-side mirror would be wrong on some of them.
   !!
   !! ### Output stays in Fortran
   !!
   !! `term_write` writes from Fortran; the C file never touches stdout. Two
   !! runtimes buffering the same stream is how a screen ends up garbled, and
   !! the way to avoid it is to have only one of them write.
   !!
   !! ### Restoring the terminal
   !!
   !! A program that leaves the shell in raw mode has committed the most
   !! user-hostile failure available: no echo, no line editing, and the user
   !! cannot even see what they type to fix it.
   !!
   !! `term_raw_enter` therefore registers an `atexit` handler the first time
   !! it succeeds, which covers a normal return, `stop`, and `error stop` --
   !! libgfortran's `error stop` exits through `exit()`. It also installs
   !! SIGINT, SIGTERM and SIGHUP handlers that restore the mode and then
   !! re-raise with the default disposition, so the process still dies of the
   !! signal it was sent rather than hanging.
   !!
   !! A segfault restores nothing. Nothing can. Run `reset`.
   !!
   !! ### Quick start
   !!
   !! ```fortran
   !! use pic_term
   !! use pic_ansi
   !!
   !! type(error_t) :: err
   !! character(len=64) :: buf
   !! integer(default_int) :: n, rows, cols
   !!
   !! if (.not. term_is_tty(TERM_STDIN)) return   ! piped: do not draw
   !! call term_enable_vt(err)
   !! call term_raw_enter(err)
   !! if (.haserror. err) return
   !!
   !! call term_size(rows, cols, err)
   !! call term_read(buf, n, 100_default_int, err)   ! 100 ms timeout
   !!
   !! call term_raw_leave()
   !! ```
   use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_char, c_null_char
   use pic_types, only: default_int, int64
   use pic_error, only: error_t, ERROR_IO, ERROR_VALIDATION
   implicit none
   private

   public :: TERM_STDIN, TERM_STDOUT, TERM_STDERR
   public :: sleep_ms
   public :: term_is_tty
   public :: term_size
   public :: term_enable_vt
   public :: term_raw_enter
   public :: term_raw_leave
   public :: term_raw_is_active
   public :: term_read
   public :: term_write

   integer(default_int), parameter :: TERM_STDIN = 0_default_int
      !! Selects standard input, for `term_is_tty`.
   integer(default_int), parameter :: TERM_STDOUT = 1_default_int
      !! Selects standard output.
   integer(default_int), parameter :: TERM_STDERR = 2_default_int
      !! Selects standard error.

   integer(c_int), parameter :: STATUS_OK = 0_c_int
      !! The C side succeeded.
   integer(c_int), parameter :: STATUS_NOT_A_TTY = 1_c_int
      !! The stream is not a terminal.
   integer(c_int), parameter :: STATUS_UNAVAILABLE = 2_c_int
      !! The question has no answer here, for example the size when there is
      !! no terminal and no `LINES`/`COLUMNS`.
   integer(c_int), parameter :: STATUS_FAILED = 3_c_int
      !! The system call failed.
   integer(c_int), parameter :: STATUS_TIMEOUT = 4_c_int
      !! A timed read expired with nothing to report.

   interface
      function c_sleep_ms(ms) bind(c, name="pic_term_sleep_ms") result(status)
         import :: c_int, c_int64_t
         implicit none
         integer(c_int64_t), value :: ms
         integer(c_int) :: status
      end function c_sleep_ms

      function c_isatty(stream) bind(c, name="pic_term_isatty") result(r)
         import :: c_int
         implicit none
         integer(c_int), value :: stream
         integer(c_int) :: r
      end function c_isatty

      function c_size(rows, cols) bind(c, name="pic_term_size") result(status)
         import :: c_int
         implicit none
         integer(c_int), intent(out) :: rows
         integer(c_int), intent(out) :: cols
         integer(c_int) :: status
      end function c_size

      function c_enable_vt() bind(c, name="pic_term_enable_vt") result(status)
         import :: c_int
         implicit none
         integer(c_int) :: status
      end function c_enable_vt

      function c_raw_enter() bind(c, name="pic_term_raw_enter") result(status)
         import :: c_int
         implicit none
         integer(c_int) :: status
      end function c_raw_enter

      function c_raw_leave() bind(c, name="pic_term_raw_leave") result(status)
         import :: c_int
         implicit none
         integer(c_int) :: status
      end function c_raw_leave

      function c_raw_is_active() bind(c, name="pic_term_raw_is_active") result(r)
         import :: c_int
         implicit none
         integer(c_int) :: r
      end function c_raw_is_active

      function c_read(buf, cap, timeout_ms, nread) bind(c, name="pic_term_read") result(status)
         import :: c_int, c_char
         implicit none
         integer(c_int), value :: cap
         ! Explicit shape rather than `buf(*)`: `cap` is already a value
         ! dummy, so the extent is known here, and an assumed-size dummy in
         ! an interop interface hides exactly the length mistake that turns
         ! into a buffer overrun on the C side.
         character(kind=c_char), intent(out) :: buf(cap)
         integer(c_int), value :: timeout_ms
         integer(c_int), intent(out) :: nread
         integer(c_int) :: status
      end function c_read
   end interface

contains

   subroutine sleep_ms(ms, err)
      !! Sleep for at least `ms` milliseconds.
      !!
      !! On POSIX this loops on `EINTR` with the time remaining, so a signal
      !! arriving mid-sleep does not silently cut it short. A non-positive
      !! duration returns at once and is not an error.
      integer(int64), intent(in) :: ms
         !! Milliseconds to sleep.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_IO` if the system call fails.

      integer(c_int) :: status

      status = c_sleep_ms(int(ms, c_int64_t))
      if (status /= STATUS_OK .and. present(err)) then
         call err%set(ERROR_IO, "pic_term: sleep failed")
      end if
   end subroutine sleep_ms

   function term_is_tty(stream) result(r)
      !! Whether `stream` is a terminal.
      !!
      !! The first thing a program that draws should ask. Output redirected to
      !! a file or a pipe must not be decorated with escape sequences, and
      !! input from a pipe cannot be put into raw mode.
      integer(default_int), intent(in) :: stream
         !! One of `TERM_STDIN`, `TERM_STDOUT`, `TERM_STDERR`.
      logical :: r

      r = c_isatty(int(stream, c_int)) /= 0_c_int
   end function term_is_tty

   subroutine term_size(rows, cols, err)
      !! The terminal's size in character cells.
      !!
      !! Asks the terminal first, then `LINES` and `COLUMNS`, and then gives
      !! up. It does not invent 24x80: a caller told the size is unavailable
      !! can pick a fallback knowingly, where one handed a plausible lie
      !! cannot.
      integer(default_int), intent(out) :: rows
         !! Rows, or 0 when unavailable.
      integer(default_int), intent(out) :: cols
         !! Columns, or 0 when unavailable.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_IO` when the size cannot be determined.

      integer(c_int) :: c_rows, c_cols, status

      status = c_size(c_rows, c_cols)
      rows = int(c_rows, default_int)
      cols = int(c_cols, default_int)
      if (status /= STATUS_OK .and. present(err)) then
         call err%set(ERROR_IO, "pic_term: terminal size is not available")
      end if
   end subroutine term_size

   subroutine term_enable_vt(err)
      !! Make the terminal understand ANSI escape sequences.
      !!
      !! A no-op on POSIX, where terminals have understood them since before
      !! the standard existed. On Windows it turns on
      !! `ENABLE_VIRTUAL_TERMINAL_PROCESSING` and switches the output code
      !! page to UTF-8, which is what lets one set of escape builders serve
      !! every platform.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_IO` when the console mode cannot be changed.

      integer(c_int) :: status

      status = c_enable_vt()
      if (status /= STATUS_OK .and. present(err)) then
         if (status == STATUS_NOT_A_TTY) then
            call err%set(ERROR_IO, "pic_term: not a terminal, cannot enable VT processing")
         else
            call err%set(ERROR_IO, "pic_term: could not enable VT processing")
         end if
      end if
   end subroutine term_enable_vt

   subroutine term_raw_enter(err)
      !! Put the terminal into raw mode: no echo, no line buffering, no signal
      !! keys.
      !!
      !! Idempotent. The first success registers the handlers described in the
      !! module documentation, so that the mode is restored on exit, on `stop`,
      !! on `error stop` and on SIGINT, SIGTERM or SIGHUP.
      !!
      !! Fails cleanly with `ERROR_IO` when standard input is not a terminal,
      !! which is what happens under a test harness or a pipe. It does not
      !! abort, so a program can offer a non-interactive path instead.
      !!
      !! On Windows this also sets `ENABLE_VIRTUAL_TERMINAL_INPUT`, so arrow
      !! keys arrive as the same `ESC [ A` bytes as on POSIX and `pic_ansi`'s
      !! `decode_keys` is the single decoder everywhere.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_IO` when raw mode cannot be entered.

      integer(c_int) :: status

      status = c_raw_enter()
      if (status /= STATUS_OK .and. present(err)) then
         if (status == STATUS_NOT_A_TTY) then
            call err%set(ERROR_IO, "pic_term: standard input is not a terminal")
         else
            call err%set(ERROR_IO, "pic_term: could not enter raw mode")
         end if
      end if
   end subroutine term_raw_enter

   subroutine term_raw_leave()
      !! Restore the terminal mode saved by `term_raw_enter`.
      !!
      !! Idempotent, and safe to call when raw mode was never entered. It
      !! takes no `err`: there is nothing a caller could usefully do about a
      !! failure to restore, and the handlers will try again on the way out.
      integer(c_int) :: status

      status = c_raw_leave()
      if (status /= STATUS_OK) return
   end subroutine term_raw_leave

   function term_raw_is_active() result(r)
      !! Whether raw mode is currently on.
      logical :: r

      r = c_raw_is_active() /= 0_c_int
   end function term_raw_is_active

   subroutine term_read(buf, nread, timeout_ms, err)
      !! Read whatever bytes are available, waiting at most `timeout_ms`.
      !!
      !! Returns as soon as anything arrives, so an interactive loop stays
      !! responsive; a timeout is reported by `nread == 0` and no error, which
      !! is the ordinary idle case and not a failure. A negative timeout waits
      !! indefinitely.
      !!
      !! The bytes go straight to `pic_ansi`'s `decode_keys`, which is why
      !! this makes no attempt to interpret them.
      character(len=*), intent(out) :: buf
         !! Destination; at most `len(buf)` bytes are read.
      integer(default_int), intent(out) :: nread
         !! Bytes actually read; 0 on a timeout.
      integer(default_int), intent(in) :: timeout_ms
         !! Milliseconds to wait; negative means forever.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_IO` when the read fails. A timeout is not a failure.

      character(kind=c_char) :: raw(len(buf))
      integer(c_int) :: status, got
      integer(default_int) :: i

      buf = ""
      nread = 0_default_int
      if (len(buf) == 0) return

      status = c_read(raw, int(len(buf), c_int), int(timeout_ms, c_int), got)
      if (status == STATUS_TIMEOUT) return
      if (status /= STATUS_OK) then
         if (present(err)) call err%set(ERROR_IO, "pic_term: read failed")
         return
      end if

      nread = int(got, default_int)
      do i = 1_default_int, nread
         buf(i:i) = raw(i)
      end do
   end subroutine term_read

   subroutine term_write(text)
      !! Write `text` with no trailing newline, and flush.
      !!
      !! Done in Fortran rather than in C on purpose: two runtimes buffering
      !! the same stream is how a screen ends up garbled, so only one of them
      !! writes. The flush matters because a frame that sits in a buffer is a
      !! frame the user has not seen.
      use, intrinsic :: iso_fortran_env, only: output_unit
      character(len=*), intent(in) :: text
         !! Text to write, typically from `pic_ansi`.

      if (len(text) == 0) return
      write (output_unit, "(a)", advance="no") text
      flush (output_unit)
   end subroutine term_write

end module pic_term
