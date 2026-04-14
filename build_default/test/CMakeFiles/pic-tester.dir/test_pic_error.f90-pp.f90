# 1 "/home/runner/work/pic/pic/test/test_pic_error.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/runner/work/pic/pic/build_default//"
# 1 "/home/runner/work/pic/pic/test/test_pic_error.f90"
module test_pic_error
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_error, only: error_t, code_to_string, operator(.haserror.), &
                        SUCCESS, ERROR_GENERIC, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION, ERROR_ALLOC
   use pic_types, only: default_int
   implicit none
   private
   public :: collect_pic_error_tests

contains

   subroutine collect_pic_error_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_default_state", test_default_state), &
                  new_unittest("test_set_and_has_error", test_set_and_has_error), &
                  new_unittest("test_clear", test_clear), &
                  new_unittest("test_get_code_and_message", test_get_code_and_message), &
                  new_unittest("test_is", test_is), &
                  new_unittest("test_code_to_string", test_code_to_string), &
                  new_unittest("test_code_to_string_unknown", test_code_to_string_unknown), &
                  new_unittest("test_add_context", test_add_context), &
                  new_unittest("test_add_context_overflow", test_add_context_overflow), &
                  new_unittest("test_wrap_single", test_wrap_single), &
                  new_unittest("test_wrap_chain", test_wrap_chain), &
                  new_unittest("test_wrap_no_error", test_wrap_no_error), &
                  new_unittest("test_set_resets_chain", test_set_resets_chain), &
                  new_unittest("test_get_full_trace_no_error", test_get_full_trace_no_error), &
                  new_unittest("test_get_full_trace_with_causes", test_get_full_trace_with_causes), &
                  new_unittest("test_print_trace_to_file", test_print_trace_to_file), &
                  new_unittest("test_error_alloc_code", test_error_alloc_code), &
                  new_unittest("test_haserror_operator", test_haserror_operator) &
                  ]
   end subroutine collect_pic_error_tests

   subroutine test_default_state(error)
      !! A fresh error_t should have no error
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call check(error,.not. err%has_error(), "Default error_t should have no error")
      if (allocated(error)) return

      call check(error, err%get_code() == SUCCESS, "Default code should be SUCCESS")
      if (allocated(error)) return

      call check(error, err%get_message() == "", "Default message should be empty")
      if (allocated(error)) return

      call check(error, err%stack_depth == 0, "Default stack depth should be 0")
      if (allocated(error)) return

      call check(error, err%cause_depth == 0, "Default cause depth should be 0")
      if (allocated(error)) return
   end subroutine test_default_state

   subroutine test_set_and_has_error(error)
      !! Setting an error should make has_error true
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_IO, "disk full")

      call check(error, err%has_error(), "Should have error after set")
      if (allocated(error)) return

      call check(error, err%code == ERROR_IO, "Code should be ERROR_IO")
      if (allocated(error)) return
   end subroutine test_set_and_has_error

   subroutine test_clear(error)
      !! Clearing should reset everything
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_GENERIC, "something broke")
      call err%add_context("mod:sub")
      call err%wrap(ERROR_IO, "higher level")

      call err%clear()

      call check(error,.not. err%has_error(), "Should have no error after clear")
      if (allocated(error)) return

      call check(error, err%stack_depth == 0, "Stack should be cleared")
      if (allocated(error)) return

      call check(error, err%cause_depth == 0, "Cause chain should be cleared")
      if (allocated(error)) return
   end subroutine test_clear

   subroutine test_get_code_and_message(error)
      !! get_code and get_message should return what was set
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_PARSE, "unexpected token")

      call check(error, err%get_code() == ERROR_PARSE, "get_code should return ERROR_PARSE")
      if (allocated(error)) return

      call check(error, err%get_message() == "unexpected token", "get_message should match")
      if (allocated(error)) return
   end subroutine test_get_code_and_message

   subroutine test_is(error)
      !! is() should match the current error code
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_VALIDATION, "bad input")

      call check(error, err%is(ERROR_VALIDATION), "Should match ERROR_VALIDATION")
      if (allocated(error)) return

      call check(error,.not. err%is(ERROR_IO), "Should not match ERROR_IO")
      if (allocated(error)) return

      call check(error,.not. err%is(SUCCESS), "Should not match SUCCESS")
      if (allocated(error)) return
   end subroutine test_is

   subroutine test_code_to_string(error)
      !! code_to_string should return named strings for all known codes
      type(error_type), allocatable, intent(out) :: error

      call check(error, code_to_string(SUCCESS) == "SUCCESS", "SUCCESS name")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_GENERIC) == "ERROR_GENERIC", "ERROR_GENERIC name")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_IO) == "ERROR_IO", "ERROR_IO name")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_PARSE) == "ERROR_PARSE", "ERROR_PARSE name")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_VALIDATION) == "ERROR_VALIDATION", "ERROR_VALIDATION name")
      if (allocated(error)) return
   end subroutine test_code_to_string

   subroutine test_code_to_string_unknown(error)
      !! Unknown codes should return "UNKNOWN"
      type(error_type), allocatable, intent(out) :: error

      call check(error, code_to_string(999_default_int) == "UNKNOWN", "Unknown code should be UNKNOWN")
      if (allocated(error)) return
   end subroutine test_code_to_string_unknown

   subroutine test_add_context(error)
      !! add_context should build a stack trace
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_IO, "read failed")
      call err%add_context("pic_io:read_file")
      call err%add_context("pic_config:load")

      call check(error, err%stack_depth == 2, "Should have 2 stack entries")
      if (allocated(error)) return

      call check(error, trim(err%call_stack(1)) == "pic_io:read_file", "First context should match")
      if (allocated(error)) return

      call check(error, trim(err%call_stack(2)) == "pic_config:load", "Second context should match")
      if (allocated(error)) return
   end subroutine test_add_context

   subroutine test_add_context_overflow(error)
      !! Adding more than MAX_STACK_DEPTH contexts should not crash
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(default_int) :: i

      call err%set(ERROR_GENERIC, "deep stack")

      do i = 1, 25
         call err%add_context("level")
      end do

      call check(error, err%stack_depth == 20, "Stack depth should cap at MAX_STACK_DEPTH")
      if (allocated(error)) return
   end subroutine test_add_context_overflow

   subroutine test_wrap_single(error)
      !! wrap should push original error into cause chain
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_IO, "file not found")
      call err%wrap(ERROR_PARSE, "failed to load config")

      call check(error, err%code == ERROR_PARSE, "Top-level code should be ERROR_PARSE")
      if (allocated(error)) return

      call check(error, err%get_message() == "failed to load config", "Top-level message should match")
      if (allocated(error)) return

      call check(error, err%cause_depth == 1, "Should have 1 cause")
      if (allocated(error)) return

      call check(error, err%cause_codes(1) == ERROR_IO, "Cause code should be ERROR_IO")
      if (allocated(error)) return

      call check(error, trim(err%cause_messages(1)) == "file not found", "Cause message should match")
      if (allocated(error)) return
   end subroutine test_wrap_single

   subroutine test_wrap_chain(error)
      !! Multiple wraps should build a cause chain
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_IO, "disk read error")
      call err%wrap(ERROR_PARSE, "bad json")
      call err%wrap(ERROR_VALIDATION, "invalid config")

      call check(error, err%code == ERROR_VALIDATION, "Top-level should be ERROR_VALIDATION")
      if (allocated(error)) return

      call check(error, err%cause_depth == 2, "Should have 2 causes")
      if (allocated(error)) return

      ! Most recent cause (the wrap that pushed ERROR_PARSE)
      call check(error, err%cause_codes(2) == ERROR_PARSE, "Second cause should be ERROR_PARSE")
      if (allocated(error)) return

      ! Original cause
      call check(error, err%cause_codes(1) == ERROR_IO, "First cause should be ERROR_IO")
      if (allocated(error)) return
   end subroutine test_wrap_chain

   subroutine test_wrap_no_error(error)
      !! Wrapping a non-error should be a no-op
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%wrap(ERROR_IO, "this should not set an error")

      call check(error,.not. err%has_error(), "Wrapping no error should remain no error")
      if (allocated(error)) return

      call check(error, err%cause_depth == 0, "Cause chain should be empty")
      if (allocated(error)) return
   end subroutine test_wrap_no_error

   subroutine test_set_resets_chain(error)
      !! Calling set after wrap should reset the cause chain
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_IO, "original")
      call err%wrap(ERROR_PARSE, "wrapped")
      call err%add_context("some:location")

      ! Now set a fresh error
      call err%set(ERROR_GENERIC, "fresh error")

      call check(error, err%cause_depth == 0, "set should reset cause chain")
      if (allocated(error)) return

      call check(error, err%stack_depth == 0, "set should reset stack")
      if (allocated(error)) return

      call check(error, err%get_message() == "fresh error", "Message should be the new one")
      if (allocated(error)) return
   end subroutine test_set_resets_chain

   subroutine test_get_full_trace_no_error(error)
      !! get_full_trace on a clean error should return empty string
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call check(error, err%get_full_trace() == "", "No-error trace should be empty")
      if (allocated(error)) return
   end subroutine test_get_full_trace_no_error

   subroutine test_get_full_trace_with_causes(error)
      !! get_full_trace should contain the error, causes, and stack
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=:), allocatable :: trace

      call err%set(ERROR_IO, "connection refused")
      call err%add_context("pic_net:connect")
      call err%wrap(ERROR_GENERIC, "service unavailable")
      call err%add_context("pic_app:run")

      trace = err%get_full_trace()

      ! Check that key pieces are present in the trace
      call check(error, index(trace, "ERROR_GENERIC") > 0, "Trace should contain top-level code name")
      if (allocated(error)) return

      call check(error, index(trace, "service unavailable") > 0, "Trace should contain top-level message")
      if (allocated(error)) return

      call check(error, index(trace, "Caused by") > 0, "Trace should contain cause header")
      if (allocated(error)) return

      call check(error, index(trace, "connection refused") > 0, "Trace should contain original message")
      if (allocated(error)) return

      call check(error, index(trace, "Call stack") > 0, "Trace should contain stack header")
      if (allocated(error)) return
   end subroutine test_get_full_trace_with_causes

   subroutine test_print_trace_to_file(error)
      !! print_trace should write to a file unit and contain expected output
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=*), parameter :: test_filename = "test_error_trace.log"
      character(len=256) :: line
      integer(default_int) :: unit_num, ios
      logical :: found_code, found_message, found_cause, file_exists

      call err%set(ERROR_IO, "write failed")
      call err%wrap(ERROR_GENERIC, "save error")

      open (newunit=unit_num, file=test_filename, status="replace", action="write")
      call err%print_trace(unit_num)
      close (unit_num)

      ! Read back and verify contents
      found_code = .false.
      found_message = .false.
      found_cause = .false.

      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read_loop: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read_loop
         if (index(line, "ERROR_GENERIC") > 0) found_code = .true.
         if (index(line, "save error") > 0) found_message = .true.
         if (index(line, "Caused by") > 0 .and. index(line, "write failed") > 0) found_cause = .true.
      end do read_loop
      close (unit_num)

      call check(error, found_code, "File should contain error code name")
      if (allocated(error)) return

      call check(error, found_message, "File should contain error message")
      if (allocated(error)) return

      call check(error, found_cause, "File should contain cause chain")
      if (allocated(error)) return

      ! Clean up
      inquire (file=test_filename, exist=file_exists)
      if (file_exists) then
         open (newunit=unit_num, file=test_filename, status="old", action="read")
         close (unit_num, status="delete")
      end if
   end subroutine test_print_trace_to_file

   subroutine test_error_alloc_code(error)
      !! Test ERROR_ALLOC code and code_to_string
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%set(ERROR_ALLOC, "failed to allocate array")

      call check(error, err%has_error(), "Should have error after set")
      if (allocated(error)) return

      call check(error, err%is(ERROR_ALLOC), "Should match ERROR_ALLOC")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_ALLOC) == "ERROR_ALLOC", "ERROR_ALLOC name")
      if (allocated(error)) return
   end subroutine test_error_alloc_code

   subroutine test_haserror_operator(error)
      !! Test .haserror. operator for checking error state
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      ! Fresh error should not have error
      call check(error,.not. (.haserror.err), "Fresh error should be false with operator")
      if (allocated(error)) return

      ! Set an error
      call err%set(ERROR_IO, "test error")

      call check(error, .haserror.err, "Error should be true with operator")
      if (allocated(error)) return

      ! Clear and check again
      call err%clear()

      call check(error,.not. (.haserror.err), "Cleared error should be false with operator")
      if (allocated(error)) return
   end subroutine test_haserror_operator

end module test_pic_error
