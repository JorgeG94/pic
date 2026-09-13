module test_pic_error
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_error, only: error_t, code_to_string, operator(.haserror.), &
                        SUCCESS, ERROR_GENERIC, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION, ERROR_ALLOC, &
                        ERROR_INTERNAL, ERROR_BOUNDS, PIC_ERROR_CODE_MAX, is_pic_error_code, error_raise
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
                  new_unittest("test_haserror_operator", test_haserror_operator), &
                  new_unittest("test_new_error_codes", test_new_error_codes), &
                  new_unittest("test_code_to_string_user_name", test_code_to_string_user_name), &
                  new_unittest("test_is_pic_error_code", test_is_pic_error_code), &
                  new_unittest("test_add_context_long_location", test_add_context_long_location), &
                  new_unittest("test_add_context_exact_fit", test_add_context_exact_fit), &
                  new_unittest("test_wrap_long_cause_message", test_wrap_long_cause_message), &
                  new_unittest("test_wrap_deep_chain_omission", test_wrap_deep_chain_omission), &
                  new_unittest("test_print_trace_omission", test_print_trace_omission), &
                  new_unittest("test_no_message_branches", test_no_message_branches), &
                  new_unittest("test_error_raise_present", test_error_raise_present), &
                  new_unittest("test_error_raise_absent", test_error_raise_absent), &
                  new_unittest("test_purity_is_preserved", test_purity_is_preserved), &
                  new_unittest("test_fatal_without_error", test_fatal_without_error), &
                  new_unittest("test_wrap_missing_message", test_wrap_missing_message), &
                  new_unittest("test_print_trace_stack_and_no_message", test_print_trace_stack_and_no_message), &
                  new_unittest("test_fatal_without_error_returns", test_fatal_without_error_returns) &
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

   subroutine test_new_error_codes(error)
      !! ERROR_INTERNAL and ERROR_BOUNDS should behave like any other code
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call check(error, code_to_string(ERROR_INTERNAL) == "ERROR_INTERNAL", "ERROR_INTERNAL name")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_BOUNDS) == "ERROR_BOUNDS", "ERROR_BOUNDS name")
      if (allocated(error)) return

      call check(error, ERROR_INTERNAL /= ERROR_BOUNDS, "New codes must be distinct")
      if (allocated(error)) return

      call err%set(ERROR_INTERNAL, "invariant violated: heap order broken")
      call check(error, err%is(ERROR_INTERNAL), "Should match ERROR_INTERNAL")
      if (allocated(error)) return

      call err%set(ERROR_BOUNDS, "index 11 out of range 1:10")
      call check(error, err%is(ERROR_BOUNDS), "Should match ERROR_BOUNDS")
      if (allocated(error)) return
   end subroutine test_new_error_codes

   subroutine test_code_to_string_user_name(error)
      !! A downstream code can supply its own name for codes PIC does not know
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: mylib_socket = PIC_ERROR_CODE_MAX + 1

      call check(error, code_to_string(mylib_socket, "MYLIB_ERROR_SOCKET") == "MYLIB_ERROR_SOCKET", &
                 "Unknown code should use the caller-supplied name")
      if (allocated(error)) return

      call check(error, code_to_string(mylib_socket) == "UNKNOWN", &
                 "Unknown code without a name should stay UNKNOWN")
      if (allocated(error)) return

      call check(error, code_to_string(ERROR_IO, "MYLIB_ERROR_SOCKET") == "ERROR_IO", &
                 "A known PIC code must ignore user_name")
      if (allocated(error)) return
   end subroutine test_code_to_string_user_name

   subroutine test_is_pic_error_code(error)
      !! The reserved range is 0 .. PIC_ERROR_CODE_MAX inclusive
      type(error_type), allocatable, intent(out) :: error

      call check(error, is_pic_error_code(SUCCESS), "SUCCESS is reserved")
      if (allocated(error)) return

      call check(error, is_pic_error_code(ERROR_BOUNDS), "ERROR_BOUNDS is reserved")
      if (allocated(error)) return

      call check(error, is_pic_error_code(PIC_ERROR_CODE_MAX), "Top of the range is reserved")
      if (allocated(error)) return

      call check(error,.not. is_pic_error_code(PIC_ERROR_CODE_MAX + 1), "First user code is not reserved")
      if (allocated(error)) return

      call check(error,.not. is_pic_error_code(-1_default_int), "Negative codes are not reserved")
      if (allocated(error)) return
   end subroutine test_is_pic_error_code

   subroutine test_add_context_long_location(error)
      !! A location longer than MAX_LOCATION_LEN must keep its line number
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=:), allocatable :: long_location

      ! Mimics __FILE__//":"//to_char(__LINE__) in a deep CI build tree
      long_location = "/builds/runner/work/pic/pic/"//repeat("deep_dir/", 18)// &
                      "src/lib/core/sort/pic_sorting_introsort.f90:1234"

      call check(error, len(long_location) > 128, "Test location must exceed MAX_LOCATION_LEN")
      if (allocated(error)) return

      call err%set(ERROR_INTERNAL, "partition invariant broken")
      call err%add_context(long_location)

      call check(error, err%stack_depth == 1, "Should have recorded one frame")
      if (allocated(error)) return

      call check(error, index(trim(err%call_stack(1)), ":1234") > 0, &
                 "Line number must survive truncation of a long location")
      if (allocated(error)) return

      call check(error, err%call_stack(1) (1:3) == "...", "Truncation must be marked at the front")
      if (allocated(error)) return

      call check(error, len_trim(err%call_stack(1)) == 128, "Truncated location should fill the slot")
      if (allocated(error)) return

      call check(error, index(err%get_full_trace(), ":1234") > 0, "Full trace must show the line number")
      if (allocated(error)) return
   end subroutine test_add_context_long_location

   subroutine test_add_context_exact_fit(error)
      !! A location exactly MAX_LOCATION_LEN long must not be marked truncated
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=:), allocatable :: location

      location = repeat("b", 123)//":9999"

      call err%set(ERROR_IO, "boundary case")
      call err%add_context(location)

      call check(error, trim(err%call_stack(1)) == location, "Exact-length location must be stored verbatim")
      if (allocated(error)) return
   end subroutine test_add_context_exact_fit

   subroutine test_wrap_long_cause_message(error)
      !! wrap must mark a cause message it had to clip
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=:), allocatable :: long_message
      integer(default_int) :: stored_len

      long_message = repeat("y", 300)

      call err%set(ERROR_IO, long_message)
      call check(error, len(err%get_message()) == 300, "Top-level message must not be truncated")
      if (allocated(error)) return

      call err%wrap(ERROR_PARSE, "could not read configuration")

      stored_len = len_trim(err%cause_messages(1))
      call check(error, stored_len == 256, "Clipped cause message should fill the slot")
      if (allocated(error)) return

      call check(error, err%cause_messages(1) (stored_len - 2:stored_len) == "...", &
                 "Clipped cause message must end with the truncation marker")
      if (allocated(error)) return

      call check(error, err%cause_messages(1) (1:253) == repeat("y", 253), &
                 "Clipped cause message must keep its head")
      if (allocated(error)) return

      call check(error, index(err%get_full_trace(), "...") > 0, "Trace must show the truncation marker")
      if (allocated(error)) return
   end subroutine test_wrap_long_cause_message

   subroutine test_wrap_deep_chain_omission(error)
      !! Wrapping past MAX_CAUSE_DEPTH keeps the root cause and reports losses
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=:), allocatable :: trace
      character(len=16) :: level_str
      integer(default_int) :: i

      call err%set(ERROR_IO, "root cause: device not ready")
      do i = 1, 12
         write (level_str, "(I0)") i
         call err%wrap(ERROR_GENERIC, "layer "//trim(level_str))
      end do

      call check(error, err%cause_depth == 8, "Cause chain should cap at MAX_CAUSE_DEPTH")
      if (allocated(error)) return

      call check(error, err%omitted_causes == 4, "Four intermediate causes should be recorded as omitted")
      if (allocated(error)) return

      trace = err%get_full_trace()

      call check(error, index(trace, "root cause: device not ready") > 0, &
                 "Root cause must survive a deep chain")
      if (allocated(error)) return

      call check(error, index(trace, "omitted") > 0, "Trace must report that layers were omitted")
      if (allocated(error)) return

      call check(error, index(trace, "4 intermediate cause(s) omitted") > 0, &
                 "Trace must report how many layers were omitted")
      if (allocated(error)) return

      call check(error, index(trace, "layer 12") > 0, "Outermost layer must be the top-level error")
      if (allocated(error)) return

      ! clear and set must both reset the omission counter
      call err%clear()
      call check(error, err%omitted_causes == 0, "clear should reset omitted_causes")
      if (allocated(error)) return

      call err%set(ERROR_IO, "root cause")
      do i = 1, 12
         call err%wrap(ERROR_GENERIC, "layer")
      end do
      call err%set(ERROR_PARSE, "fresh")
      call check(error, err%omitted_causes == 0, "set should reset omitted_causes")
      if (allocated(error)) return
   end subroutine test_wrap_deep_chain_omission

   subroutine test_print_trace_omission(error)
      !! print_trace must also report omitted causes
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=*), parameter :: test_filename = "test_error_omission.log"
      character(len=256) :: line
      integer(default_int) :: unit_num, ios, i
      logical :: found_omitted, found_root, found_stack

      call err%set(ERROR_IO, "root cause here")
      do i = 1, 10
         call err%wrap(ERROR_GENERIC, "wrapper")
      end do
      call err%add_context("test_pic_error:test_print_trace_omission:42")

      open (newunit=unit_num, file=test_filename, status="replace", action="write")
      call err%print_trace(unit_num)
      close (unit_num)

      found_omitted = .false.
      found_root = .false.
      found_stack = .false.
      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read_loop: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read_loop
         if (index(line, "intermediate cause(s) omitted") > 0) found_omitted = .true.
         if (index(line, "root cause here") > 0) found_root = .true.
         if (index(line, "test_print_trace_omission:42") > 0) found_stack = .true.
      end do read_loop
      close (unit_num, status="delete")

      call check(error, found_omitted, "print_trace should report omitted causes")
      if (allocated(error)) return

      call check(error, found_root, "print_trace should still show the root cause")
      if (allocated(error)) return

      call check(error, found_stack, "print_trace should print the call stack")
      if (allocated(error)) return
   end subroutine test_print_trace_omission

   subroutine test_no_message_branches(error)
      !! An error_t whose code was set directly has no message allocated
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=*), parameter :: test_filename = "test_error_nomsg.log"
      character(len=256) :: line
      integer(default_int) :: unit_num, ios
      logical :: found_nomsg

      ! Deliberately bypass set(): code is public, message stays unallocated
      err%code = ERROR_BOUNDS

      call check(error, err%get_full_trace() == "ERROR_BOUNDS: ", &
                 "Trace of a message-less error should be just the code")
      if (allocated(error)) return

      call err%wrap(ERROR_INTERNAL, "wrapped a message-less error")
      call check(error, trim(err%cause_messages(1)) == "(no message)", &
                 "A message-less cause should render as (no message)")
      if (allocated(error)) return

      ! print_trace path for a message-less top-level error
      err%code = ERROR_BOUNDS
      if (allocated(err%message)) deallocate (err%message)

      open (newunit=unit_num, file=test_filename, status="replace", action="write")
      call err%print_trace(unit_num)
      close (unit_num)

      found_nomsg = .false.
      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read_loop: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read_loop
         if (index(line, "(no message)") > 0) found_nomsg = .true.
      end do read_loop
      close (unit_num, status="delete")

      call check(error, found_nomsg, "print_trace should print (no message)")
      if (allocated(error)) return
   end subroutine test_no_message_branches

   subroutine test_error_raise_present(error)
      !! error_raise sets the error when the caller passed one
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call raising_pure_routine(ERROR_BOUNDS, err)

      call check(error, err%has_error(), "error_raise should set a present err")
      if (allocated(error)) return

      call check(error, err%is(ERROR_BOUNDS), "error_raise should set the requested code")
      if (allocated(error)) return

      call check(error, err%get_message() == "raised from a pure procedure", &
                 "error_raise should set the requested message")
      if (allocated(error)) return

      ! calling it directly, not through the pure wrapper
      call err%clear()
      call error_raise(err, ERROR_INTERNAL, "direct call")
      call check(error, err%is(ERROR_INTERNAL), "Direct error_raise should work too")
      if (allocated(error)) return
   end subroutine test_error_raise_present

   subroutine test_error_raise_absent(error)
      !! error_raise is a silent no-op when the caller omitted err
      type(error_type), allocatable, intent(out) :: error

      ! Must not crash; the failure is silently dropped, which is documented
      call raising_pure_routine(ERROR_BOUNDS)

      call check(error, .true., "error_raise with an absent err must be a no-op")
      if (allocated(error)) return
   end subroutine test_error_raise_absent

   subroutine test_purity_is_preserved(error)
      !! Compile-time guard: the sort migration needs set/error_raise to stay
      !! usable from pure and pure recursive procedures. If the helpers below
      !! stop compiling, that migration is dead.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call pure_recursive_setter(3_default_int, err)

      call check(error, err%is(ERROR_INTERNAL), "pure recursive set should have set the code")
      if (allocated(error)) return

      call check(error, err%get_message() == "set from a pure recursive procedure", &
                 "pure recursive set should have set the message")
      if (allocated(error)) return

      call check(error, err%stack_depth == 3, "pure recursive add_context should have built a stack")
      if (allocated(error)) return
   end subroutine test_purity_is_preserved

   subroutine test_fatal_without_error(error)
      !! fatal on a clean error_t must return instead of stopping
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%fatal()
      call err%fatal(exit_code=2_default_int)
      call err%clear()
      call err%fatal()

      call check(error,.not. err%has_error(), "fatal on a clean error must be a no-op")
      if (allocated(error)) return
   end subroutine test_fatal_without_error

   pure recursive subroutine pure_recursive_setter(depth, err)
      !! Purity guard: mutating an error_t from a pure recursive procedure.
      !! Note there is deliberately no local error_t here - 4672 bytes per
      !! recursion frame is not acceptable.
      integer(default_int), intent(in) :: depth
      type(error_t), intent(inout) :: err

      if (depth <= 0) then
         call err%set(ERROR_INTERNAL, "set from a pure recursive procedure")
         return
      end if

      call pure_recursive_setter(depth - 1, err)
      call err%add_context("test_pic_error:pure_recursive_setter")
   end subroutine pure_recursive_setter

   pure subroutine raising_pure_routine(code, err)
      !! Purity guard: the optional-err propagation pattern the sort
      !! migration uses at ~146 sites.
      integer(default_int), intent(in) :: code
      type(error_t), intent(inout), optional :: err

      call error_raise(err, code, "raised from a pure procedure")
   end subroutine raising_pure_routine

   subroutine test_wrap_missing_message(error)
      !! Wrapping an error whose message was never allocated should record
      !! the "(no message)" placeholder in the cause chain
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      ! Set the code directly so that %message stays unallocated
      err%code = ERROR_IO

      call err%wrap(ERROR_PARSE, "outer context")

      call check(error, err%cause_depth == 1, "One cause should have been recorded")
      if (allocated(error)) return

      call check(error, trim(err%cause_messages(1)) == "(no message)", &
                 "Missing cause message should be replaced by the placeholder")
      if (allocated(error)) return

      call check(error, err%cause_codes(1) == ERROR_IO, "Cause code should be the original code")
      if (allocated(error)) return

      call check(error, err%get_code() == ERROR_PARSE, "Top level code should be the wrapping code")
      if (allocated(error)) return
   end subroutine test_wrap_missing_message

   subroutine test_print_trace_stack_and_no_message(error)
      !! print_trace should emit the "(no message)" placeholder and the call
      !! stack section when a stack has been accumulated
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      character(len=*), parameter :: test_filename = "test_error_trace_stack.log"
      character(len=256) :: line
      integer(default_int) :: unit_num, ios
      logical :: found_no_message, found_header, found_inner, found_outer, file_exists

      ! Set the code directly so that %message stays unallocated
      err%code = ERROR_VALIDATION
      call err%add_context("pic_deep:inner")
      call err%add_context("pic_shallow:outer")

      open (newunit=unit_num, file=test_filename, status="replace", action="write")
      call err%print_trace(unit_num)
      close (unit_num)

      found_no_message = .false.
      found_header = .false.
      found_inner = .false.
      found_outer = .false.

      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read_loop: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read_loop
         if (index(line, "(no message)") > 0) found_no_message = .true.
         if (index(line, "Call stack (most recent first):") > 0) found_header = .true.
         if (index(line, "[1]") > 0 .and. index(line, "pic_deep:inner") > 0) found_inner = .true.
         if (index(line, "[2]") > 0 .and. index(line, "pic_shallow:outer") > 0) found_outer = .true.
      end do read_loop
      close (unit_num)

      call check(error, found_no_message, "Trace should contain the (no message) placeholder")
      if (allocated(error)) return

      call check(error, found_header, "Trace should contain the call stack header")
      if (allocated(error)) return

      call check(error, found_inner, "Trace should list the first context entry")
      if (allocated(error)) return

      call check(error, found_outer, "Trace should list the second context entry")
      if (allocated(error)) return

      inquire (file=test_filename, exist=file_exists)
      if (file_exists) then
         open (newunit=unit_num, file=test_filename, status="old", action="read")
         close (unit_num, status="delete")
      end if
   end subroutine test_print_trace_stack_and_no_message

   subroutine test_fatal_without_error_returns(error)
      !! fatal() on a clean error_t must return instead of stopping
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call err%fatal()

      call check(error,.not. err%has_error(), "fatal() must leave a clean error_t untouched")
      if (allocated(error)) return

      call err%set(ERROR_IO, "boom")
      call err%clear()
      call err%fatal()

      call check(error, err%get_code() == SUCCESS, "fatal() on a cleared error must return")
      if (allocated(error)) return
   end subroutine test_fatal_without_error_returns

end module test_pic_error
