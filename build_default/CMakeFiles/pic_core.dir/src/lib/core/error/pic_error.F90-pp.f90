# 1 "/home/runner/work/pic/pic/src/lib/core/error/pic_error.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/runner/work/pic/pic/build_default//"
# 1 "/home/runner/work/pic/pic/src/lib/core/error/pic_error.F90"
! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Error handling module for pic
!! Provides a unified error type to replace stat/errmsg pairs
!! Enhanced with stack trace support for better debugging
!!
!! Preprocessor macro for automatic source location:
!!   #define PIC_ADD_CONTEXT(err) call err%add_context("/home/runner/work/pic/pic/src/lib/core/error/pic_error.F90"//":"//to_char(8))
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
   use pic_types, only: default_int
   use pic_global_definitions, only: stdout

   implicit none
   private

   public :: error_t
   public :: SUCCESS, ERROR_GENERIC, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION, ERROR_ALLOC
   public :: code_to_string
   public :: operator(.haserror.)

   !! Error codes
   integer(default_int), parameter :: SUCCESS = 0
   integer(default_int), parameter :: ERROR_GENERIC = 1
   integer(default_int), parameter :: ERROR_IO = 2
   integer(default_int), parameter :: ERROR_PARSE = 3
   integer(default_int), parameter :: ERROR_VALIDATION = 4
   integer(default_int), parameter :: ERROR_ALLOC = 5

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

   pure function code_to_string(code) result(name)
      !! Map an error code to a human-readable name
      !!
      !! Usage: print *, code_to_string(ERROR_IO)  ! "ERROR_IO"
      integer(default_int), intent(in) :: code
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
      case default
         name = "UNKNOWN"
      end select
   end function code_to_string

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
   end subroutine error_set

   pure subroutine error_clear(self)
      !! Clear the error state, stack trace, and cause chain
      class(error_t), intent(inout) :: self
      self%code = SUCCESS
      self%stack_depth = 0
      self%cause_depth = 0
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
      class(error_t), intent(inout) :: self
      integer(default_int), intent(in) :: code
      character(len=*), intent(in) :: message

      if (.not. self%has_error()) return

      ! Push current error into cause chain
      if (self%cause_depth < MAX_CAUSE_DEPTH) then
         self%cause_depth = self%cause_depth + 1
         self%cause_codes(self%cause_depth) = self%code
         if (allocated(self%message)) then
            self%cause_messages(self%cause_depth) = self%message
         else
            self%cause_messages(self%cause_depth) = "(no message)"
         end if
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
      class(error_t), intent(inout) :: self
      character(len=*), intent(in) :: location

      if (self%stack_depth < MAX_STACK_DEPTH) then
         self%stack_depth = self%stack_depth + 1
         self%call_stack(self%stack_depth) = location
      end if
   end subroutine error_add_context

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

      ! Cause chain: prints from most-recent wrap (cause_depth) down to
      ! root cause (1), matching Rust's "caused by" display order
      do i = self%cause_depth, 1, -1
         trace = trace//new_line('a')//"  Caused by: "// &
                 trim(code_to_string(self%cause_codes(i)))//": "// &
                 trim(self%cause_messages(i))
      end do

      ! Stack trace
      if (self%stack_depth > 0) then
         trace = trace//new_line('a')//"Call stack (most recent first):"
         do i = self%stack_depth, 1, -1
            write (idx_str, '(I0)') i
            trace = trace//new_line('a')//"  ["//trim(idx_str)//"] "// &
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
      write (out_unit, '(A)', advance='no') code_to_string(self%code)//": "
      if (allocated(self%message)) then
         write (out_unit, '(A)') trim(self%message)
      else
         write (out_unit, '(A)') "(no message)"
      end if

      ! Cause chain
      do i = self%cause_depth, 1, -1
         write (out_unit, '(A)', advance='no') "  Caused by: "//trim(code_to_string(self%cause_codes(i)))//": "
         write (out_unit, '(A)') trim(self%cause_messages(i))
      end do

      ! Stack trace
      if (self%stack_depth > 0) then
         write (out_unit, '(A)') "Call stack (most recent first):"
         do i = self%stack_depth, 1, -1
            write (out_unit, '(A,I0,A)', advance='no') "  [", i, "] "
            write (out_unit, '(A)') trim(self%call_stack(i))
         end do
      end if
   end subroutine error_print_trace

   subroutine error_fatal(self, unit)
      !! Print the error trace and stop the program
      !! Use for unrecoverable errors
      !!
      !! Usage:
      !!   if (err%has_error()) call err%fatal()
      class(error_t), intent(in) :: self
      integer(default_int), intent(in), optional :: unit

      if (.not. self%has_error()) return

      call self%print_trace(unit)
      error stop 1
   end subroutine error_fatal

end module pic_error
