! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Error handling module for pic
!! Provides a unified error type to replace stat/errmsg pairs
!! Enhanced with stack trace support for better debugging
!!
!! Preprocessor macro for automatic source location:
!!   #define PIC_ADD_CONTEXT(err) call err%add_context(__FILE__//":"//to_char(__LINE__))
!! Requires: use pic_io, only: to_char
!! Requires: .F90 file extension for preprocessing
module pic_error
   !! Unified error handling with stack traces for PIC.
   !! Use error_t as an intent(out) or intent(inout) argument
   !! to propagate errors with context through call chains.
   !!
   !! Basic usage:
   !!   type(error_t) :: err
   !!   call err%set(ERROR_IO, "failed to open file")
   !!   if (err%has_error()) call err%fatal()
   !!   ! Or using operator: if (.haserror. err) call err%fatal()
   !!
   !! Error wrapping (Rust-style "caused by", outermost first):
   !!   call low_level_routine(err)
   !!   if (err%has_error()) then
   !!      call err%wrap(ERROR_PARSE, "failed to parse input")
   !!      return
   !!   end if
   !!
   !! ## Purity
   !!
   !! Every mutator (`set`, `clear`, `wrap`, `add_context`) and the
   !! `error_raise` helper are `pure`, so they can be used from the `pure`
   !! and `pure recursive` procedures that make up most of PIC's sorting and
   !! array code. The reporting procedures (`get_full_trace`, `print_trace`,
   !! `fatal`) perform I/O and are therefore necessarily impure: calling
   !! `err%fatal()` from a `pure` procedure is a compile error by design.
   !! A `pure` procedure must return the error to its caller and let an
   !! impure caller report it.
   !!
   !! ## Thread safety
   !!
   !! `error_t` carries no locking of any kind. PIC has threaded code paths
   !! (`pic_array` exposes `set_threading_mode`, and sorting can be threaded),
   !! so the rule is:
   !!
   !!   * give every thread its own `error_t` (a thread-private local, or one
   !!     element of a per-thread array), then reduce/inspect them after the
   !!     parallel region, or
   !!   * write to a single shared `error_t` only from inside a `critical`
   !!     region (or an equivalent lock).
   !!
   !! Two threads calling `set`, `wrap`, `add_context` or `clear` on the same
   !! `error_t` is a data race: `message` is an allocatable component, so
   !! concurrent writes can corrupt the heap, not merely lose a message.
   !! Reading a shared `error_t` (`has_error`, `is`, `get_code`) while another
   !! thread writes it is equally a race.
   !!
   !! ## Error code ranges
   !!
   !! PIC reserves codes `0 .. PIC_ERROR_CODE_MAX` (0-99) for itself and will
   !! only ever add new library codes inside that range. Downstream code that
   !! wants its own codes must take them at or above `PIC_ERROR_CODE_MAX + 1`
   !! (i.e. 100 and up); `is_pic_error_code` checks a candidate value.
   !! Because `code_to_string` cannot know a downstream name, pass it:
   !!
   !!   integer(default_int), parameter :: MYLIB_ERROR_SOCKET = 100
   !!   print *, code_to_string(err%get_code(), "MYLIB_ERROR_SOCKET")
   !!
   !! The `user_name` argument is only consulted for codes PIC does not know,
   !! so it is safe to pass unconditionally.
   use pic_types, only: default_int
   use pic_global_definitions, only: stdout

   implicit none
   private

   public :: error_t
   public :: SUCCESS, ERROR_GENERIC, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION, ERROR_ALLOC
   public :: ERROR_INTERNAL, ERROR_BOUNDS
   public :: PIC_ERROR_CODE_MAX
   public :: code_to_string
   public :: is_pic_error_code
   public :: error_raise
   public :: operator(.haserror.)

   !! Error codes
   integer(default_int), parameter :: SUCCESS = 0
   integer(default_int), parameter :: ERROR_GENERIC = 1
   integer(default_int), parameter :: ERROR_IO = 2
   integer(default_int), parameter :: ERROR_PARSE = 3
   integer(default_int), parameter :: ERROR_VALIDATION = 4
   integer(default_int), parameter :: ERROR_ALLOC = 5
   integer(default_int), parameter :: ERROR_INTERNAL = 6
      !! a PIC invariant was violated: this is a bug in the library itself,
      !! not bad input from the caller. Report it upstream.
   integer(default_int), parameter :: ERROR_BOUNDS = 7
      !! an index or extent was out of range

   integer(default_int), parameter :: PIC_ERROR_CODE_MAX = 99
      !! Highest error code PIC reserves for itself. Downstream libraries
      !! must number their own codes from PIC_ERROR_CODE_MAX + 1 upwards.

   interface operator(.haserror.)
      !! Operator for checking error state: if (.haserror. err) then
      module procedure error_has_error
   end interface

   !! Stack trace configuration
   integer(default_int), parameter :: MAX_STACK_DEPTH = 20
   integer(default_int), parameter :: MAX_LOCATION_LEN = 128

   !! Cause chain configuration
   integer(default_int), parameter :: MAX_CAUSE_DEPTH = 8
   integer(default_int), parameter :: MAX_CAUSE_MSG_LEN = 256

   character(len=*), parameter :: TRUNCATION_MARKER = "..."
      !! Marker used whenever a location or cause message does not fit in its
      !! fixed-length slot. Locations keep their tail (the `:line` part) and
      !! get the marker as a prefix; messages keep their head and get it as a
      !! suffix.

   !! Unified error type with stack trace support
   type :: error_t
      integer(default_int) :: code = SUCCESS
         !! Error code (0 = no error)
      character(len=:), allocatable :: message
         !! Error message

      !! Stack trace support
      integer(default_int) :: stack_depth = 0
         !! Current stack depth
      character(len=MAX_LOCATION_LEN) :: call_stack(MAX_STACK_DEPTH)
         !! Call locations

      !! Cause chain support (Rust-style "caused by")
      integer(default_int) :: cause_depth = 0
         !! Number of wrapped causes
      integer(default_int) :: cause_codes(MAX_CAUSE_DEPTH)
         !! Error codes of wrapped causes
      character(len=MAX_CAUSE_MSG_LEN) :: cause_messages(MAX_CAUSE_DEPTH)
         !! Messages of wrapped causes
      integer(default_int) :: omitted_causes = 0
         !! Number of intermediate causes dropped because the cause chain was
         !! already MAX_CAUSE_DEPTH deep. The root cause is always kept; it is
         !! the middle layers that are lost, and the traces report how many.
   contains
      procedure :: has_error => error_has_error
      procedure :: set => error_set
      procedure :: clear => error_clear
      procedure :: get_code => error_get_code
      procedure :: get_message => error_get_message
      procedure :: is => error_is
      procedure :: wrap => error_wrap
      procedure :: add_context => error_add_context
      procedure :: get_full_trace => error_get_full_trace
      procedure :: print_trace => error_print_trace
      procedure :: fatal => error_fatal
   end type error_t

contains

   pure function code_to_string(code, user_name) result(name)
      !! Map an error code to a human-readable name
      !!
      !! Usage: print *, code_to_string(ERROR_IO)  ! "ERROR_IO"
      !!
      !! `user_name` is an optional name for a code PIC does not know about,
      !! so that downstream libraries owning codes above PIC_ERROR_CODE_MAX
      !! still get readable output:
      !!   print *, code_to_string(MYLIB_ERROR_SOCKET, "MYLIB_ERROR_SOCKET")
      !! It is ignored for codes PIC recognises, so it can be passed
      !! unconditionally. Unknown codes without a `user_name` still render as
      !! "UNKNOWN".
      integer(default_int), intent(in) :: code
      character(len=*), intent(in), optional :: user_name
      character(len=:), allocatable :: name

      select case (code)
      case (SUCCESS)
         name = "SUCCESS"
      case (ERROR_GENERIC)
         name = "ERROR_GENERIC"
      case (ERROR_IO)
         name = "ERROR_IO"
      case (ERROR_PARSE)
         name = "ERROR_PARSE"
      case (ERROR_VALIDATION)
         name = "ERROR_VALIDATION"
      case (ERROR_ALLOC)
         name = "ERROR_ALLOC"
      case (ERROR_INTERNAL)
         name = "ERROR_INTERNAL"
      case (ERROR_BOUNDS)
         name = "ERROR_BOUNDS"
      case default
         if (present(user_name)) then
            name = trim(user_name)
         else
            name = "UNKNOWN"
         end if
      end select
   end function code_to_string

   pure function is_pic_error_code(code) result(reserved)
      !! True when `code` falls inside the range PIC reserves for its own
      !! error codes (0 .. PIC_ERROR_CODE_MAX). Downstream libraries can
      !! assert their own codes are outside it:
      !!   if (is_pic_error_code(MYLIB_ERROR_SOCKET)) error stop "code clash"
      integer(default_int), intent(in) :: code
      logical :: reserved
      reserved = (code >= 0) .and. (code <= PIC_ERROR_CODE_MAX)
   end function is_pic_error_code

   pure subroutine error_raise(err, code, message)
      !! Set an optional error argument, if the caller supplied one.
      !!
      !! This exists for the very common propagation pattern in PIC's `pure`
      !! numerical code, which declares `type(error_t), intent(inout),
      !! optional :: err` and would otherwise need an explicit
      !! `if (present(err))` guard at every single failure site:
      !!
      !!   pure subroutine sort_something(a, err)
      !!      type(error_t), intent(inout), optional :: err
      !!      if (size(a) < 0) then
      !!         call error_raise(err, ERROR_BOUNDS, "negative extent")
      !!         return
      !!      end if
      !!
      !! IMPORTANT LIMITATION - the error can be silently dropped. When `err`
      !! is absent this routine does nothing at all: it cannot print, and it
      !! cannot `error stop` either, because a `pure` procedure may only
      !! `error stop` with a constant message and PIC will not abort a
      !! library call on the caller's behalf. A caller that does not pass
      !! `err` therefore receives NO indication that anything went wrong, and
      !! must be able to tolerate that. If you need to be told about
      !! failures, pass `err` and check it.
      !!
      !! Like `set`, this resets any existing stack trace and cause chain on
      !! `err`; it does not wrap what was already there.
      type(error_t), intent(inout), optional :: err
      integer(default_int), intent(in) :: code
      character(len=*), intent(in) :: message

      if (present(err)) call err%set(code, message)
   end subroutine error_raise

   pure function error_has_error(self) result(has_err)
      !! Check if an error is set
      class(error_t), intent(in) :: self
      logical :: has_err
      has_err = (self%code /= SUCCESS)
   end function error_has_error

   pure function error_is(self, code) result(matches)
      !! Check if the error matches a specific error code
      !!
      !! Usage:
      !!   if (err%is(ERROR_IO)) then
      !!      ! handle IO errors specifically
      !!   end if
      class(error_t), intent(in) :: self
      integer(default_int), intent(in) :: code
      logical :: matches
      matches = (self%code == code)
   end function error_is

   pure subroutine error_set(self, code, message)
      !! Set an error with code and message
      !! Resets the stack trace and cause chain
      class(error_t), intent(inout) :: self
      integer(default_int), intent(in) :: code
      character(len=*), intent(in) :: message

      self%code = code
      self%message = trim(message)
      self%stack_depth = 0
      self%cause_depth = 0
      self%omitted_causes = 0
   end subroutine error_set

   pure subroutine error_clear(self)
      !! Clear the error state, stack trace, and cause chain
      class(error_t), intent(inout) :: self
      self%code = SUCCESS
      self%stack_depth = 0
      self%cause_depth = 0
      self%omitted_causes = 0
      if (allocated(self%message)) deallocate (self%message)
   end subroutine error_clear

   pure function error_get_code(self) result(code)
      !! Get the error code
      class(error_t), intent(in) :: self
      integer(default_int) :: code
      code = self%code
   end function error_get_code

   pure function error_get_message(self) result(message)
      !! Get the error message (without stack trace)
      class(error_t), intent(in) :: self
      character(len=:), allocatable :: message
      if (allocated(self%message)) then
         message = self%message
      else
         message = ""
      end if
   end function error_get_message

   pure subroutine error_wrap(self, code, message)
      !! Wrap the current error with a higher-level context
      !! Pushes the current error into the cause chain and sets a new
      !! top-level code and message (Rust-style "caused by")
      !!
      !! Print order: outermost wrapper first, root cause last (like Rust).
      !!
      !! Usage:
      !!   call parse_json(data, err)
      !!   if (err%has_error()) then
      !!      call err%wrap(ERROR_PARSE, "failed to load config file")
      !!      return
      !!   end if
      !!
      !! Produces:
      !!   ERROR_PARSE: failed to load config file
      !!     Caused by: ERROR_IO: could not read file "input.json"
      !!
      !! Limits. The top-level `message` is deferred-length and never
      !! truncated, but a message pushed into the cause chain has to fit a
      !! fixed MAX_CAUSE_MSG_LEN slot; anything longer keeps its head and
      !! ends with TRUNCATION_MARKER so the clipping is visible. Likewise
      !! only MAX_CAUSE_DEPTH causes are stored: the root cause is always
      !! kept and further wraps bump `omitted_causes`, which the traces
      !! report as "N intermediate cause(s) omitted", so a deep chain is
      !! never silently presented as complete.
      class(error_t), intent(inout) :: self
      integer(default_int), intent(in) :: code
      character(len=*), intent(in) :: message

      if (.not. self%has_error()) return

      ! Push current error into cause chain
      if (self%cause_depth < MAX_CAUSE_DEPTH) then
         self%cause_depth = self%cause_depth + 1
         self%cause_codes(self%cause_depth) = self%code
         if (allocated(self%message)) then
            self%cause_messages(self%cause_depth) = clip_tail(self%message)
         else
            self%cause_messages(self%cause_depth) = "(no message)"
         end if
      else
         ! Chain is full. The root cause matters most, so keep what is stored
         ! and record that a middle layer was dropped.
         self%omitted_causes = self%omitted_causes + 1
      end if

      ! Set new top-level error
      self%code = code
      self%message = trim(message)
   end subroutine error_wrap

   pure subroutine error_add_context(self, location)
      !! Add a call location to the stack trace
      !! Typically called when propagating errors upward
      !!
      !! Example:
      !!   call some_routine(..., err)
      !!   if (err%has_error()) then
      !!      call err%add_context("pic_module:my_subroutine")
      !!      return
      !!   end if
      !!
      !! Locations are stored in fixed MAX_LOCATION_LEN slots. A location
      !! that does not fit is truncated from the LEFT and prefixed with
      !! TRUNCATION_MARKER, because these are overwhelmingly
      !! `__FILE__//":"//to_char(__LINE__)` strings whose useful end is the
      !! line number: in a CI build tree the absolute path alone can exceed
      !! the slot, and clipping the tail would throw away the only part
      !! anybody reads.
      class(error_t), intent(inout) :: self
      character(len=*), intent(in) :: location

      if (self%stack_depth < MAX_STACK_DEPTH) then
         self%stack_depth = self%stack_depth + 1
         self%call_stack(self%stack_depth) = clip_head(location)
      end if
   end subroutine error_add_context

   pure function clip_head(location) result(clipped)
      !! Fit a location into MAX_LOCATION_LEN keeping its TAIL, marking any
      !! truncation with a leading TRUNCATION_MARKER.
      character(len=*), intent(in) :: location
      character(len=MAX_LOCATION_LEN) :: clipped
      integer(default_int) :: loc_len, first

      loc_len = int(len_trim(location), default_int)
      if (loc_len <= MAX_LOCATION_LEN) then
         clipped = location
      else
         first = loc_len - MAX_LOCATION_LEN + int(len(TRUNCATION_MARKER), default_int) + 1
         clipped = TRUNCATION_MARKER//location(first:loc_len)
      end if
   end function clip_head

   pure function clip_tail(message) result(clipped)
      !! Fit a cause message into MAX_CAUSE_MSG_LEN keeping its HEAD, marking
      !! any truncation with a trailing TRUNCATION_MARKER.
      character(len=*), intent(in) :: message
      character(len=MAX_CAUSE_MSG_LEN) :: clipped
      integer(default_int) :: msg_len, last

      msg_len = int(len_trim(message), default_int)
      if (msg_len <= MAX_CAUSE_MSG_LEN) then
         clipped = message
      else
         last = MAX_CAUSE_MSG_LEN - int(len(TRUNCATION_MARKER), default_int)
         clipped = message(1:last)//TRUNCATION_MARKER
      end if
   end function clip_tail

   pure function omitted_note(count) result(note)
      !! One-line notice that `count` intermediate causes were dropped
      integer(default_int), intent(in) :: count
      character(len=:), allocatable :: note
      character(len=32) :: count_str

      write (count_str, "(I0)") count
      note = "  "//TRUNCATION_MARKER//" "//trim(count_str)// &
             " intermediate cause(s) omitted "//TRUNCATION_MARKER
   end function omitted_note

   function error_get_full_trace(self) result(trace)
      !! Get complete error message with cause chain and stack trace
      !! Returns a dynamically-sized multi-line string
      class(error_t), intent(in) :: self
      character(len=:), allocatable :: trace
      character(len=32) :: idx_str
      integer(default_int) :: i

      if (.not. self%has_error()) then
         trace = ""
         return
      end if

      ! Top-level error with named code
      trace = code_to_string(self%code)//": "
      if (allocated(self%message)) then
         trace = trace//self%message
      end if

      ! Dropped middle layers sit between the top-level error and the
      ! oldest stored cause, so report them first
      if (self%omitted_causes > 0) then
         trace = trace//new_line("a")//omitted_note(self%omitted_causes)
      end if

      ! Cause chain: prints from most-recent wrap (cause_depth) down to
      ! root cause (1), matching Rust's "caused by" display order
      do i = self%cause_depth, 1, -1
         trace = trace//new_line("a")//"  Caused by: "// &
                 trim(code_to_string(self%cause_codes(i)))//": "// &
                 trim(self%cause_messages(i))
      end do

      ! Stack trace
      if (self%stack_depth > 0) then
         trace = trace//new_line("a")//"Call stack (most recent first):"
         do i = self%stack_depth, 1, -1
            write (idx_str, "(I0)") i
            trace = trace//new_line("a")//"  ["//trim(idx_str)//"] "// &
                    trim(self%call_stack(i))
         end do
      end if
   end function error_get_full_trace

   subroutine error_print_trace(self, unit)
      !! Print error with cause chain and stack trace to specified unit
      !! If unit not specified, prints to stdout
      class(error_t), intent(in) :: self
      integer(default_int), intent(in), optional :: unit
      integer(default_int) :: out_unit, i

      out_unit = stdout
      if (present(unit)) out_unit = unit

      if (.not. self%has_error()) return

      ! Top-level error with named code
      write (out_unit, "(A)", advance="no") code_to_string(self%code)//": "
      if (allocated(self%message)) then
         write (out_unit, "(A)") trim(self%message)
      else
         write (out_unit, "(A)") "(no message)"
      end if

      ! Causes dropped because the chain was full
      if (self%omitted_causes > 0) then
         write (out_unit, "(A)") omitted_note(self%omitted_causes)
      end if

      ! Cause chain
      do i = self%cause_depth, 1, -1
         write (out_unit, "(A)", advance="no") "  Caused by: "//trim(code_to_string(self%cause_codes(i)))//": "
         write (out_unit, "(A)") trim(self%cause_messages(i))
      end do

      ! Stack trace
      if (self%stack_depth > 0) then
         write (out_unit, "(A)") "Call stack (most recent first):"
         do i = self%stack_depth, 1, -1
            write (out_unit, "(A,I0,A)", advance="no") "  [", i, "] "
            write (out_unit, "(A)") trim(self%call_stack(i))
         end do
      end if
   end subroutine error_print_trace

   subroutine error_fatal(self, unit, exit_code)
      !! Print the error trace and stop the program
      !! Use for unrecoverable errors
      !!
      !! Usage:
      !!   if (err%has_error()) call err%fatal()
      !!   if (err%has_error()) call err%fatal(exit_code=2)  ! custom status
      !!
      !! `exit_code` is the process exit status, default 1. A non-default
      !! status needs F2018 variable stop codes; compilers without them can
      !! be built with -DPIC_NO_VARIABLE_STOP_CODE, which always exits 1.
      class(error_t), intent(in) :: self
      integer(default_int), intent(in), optional :: unit
      integer(default_int), intent(in), optional :: exit_code
#ifndef PIC_NO_VARIABLE_STOP_CODE
      integer(default_int) :: status
#endif

#ifndef PIC_NO_VARIABLE_STOP_CODE
      status = 1
      if (present(exit_code)) status = exit_code
#endif

      if (.not. self%has_error()) return

      call self%print_trace(unit)
#ifdef PIC_NO_VARIABLE_STOP_CODE
      error stop 1
#else
      ! int() puts the stop code in the compiler's default integer kind,
      ! which is what the standard requires even in an int64 PIC build
      error stop int(status)
#endif
   end subroutine error_fatal

end module pic_error
