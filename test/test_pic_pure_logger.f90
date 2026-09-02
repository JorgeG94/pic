module test_pic_pure_logger
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_pure_logger, only: log_buffer_type, log_entry_type, max_log_entries, &
                              pure_debug, pure_verbose, pure_large_info, pure_info, &
                              pure_performance, pure_warning, pure_error, pure_knowledge, &
                              clear_log_buffer, flush_log_buffer
   use pic_logger, only: logger_type, debug_level, error_level, large_info_level
   use pic_types, only: default_int
   use pic_global_definitions, only: logfile_unit
   implicit none
   private
   public :: collect_pic_pure_logger_tests

contains

   subroutine collect_pic_pure_logger_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_buffer_starts_empty", test_buffer_starts_empty), &
                  new_unittest("test_buffer_single_message", test_buffer_single_message), &
                  new_unittest("test_buffer_all_levels", test_buffer_all_levels), &
                  new_unittest("test_buffer_with_module_procedure", test_buffer_with_module_procedure), &
                  new_unittest("test_buffer_overflow", test_buffer_overflow), &
                  new_unittest("test_buffer_clear", test_buffer_clear), &
                  new_unittest("test_buffer_flush", test_buffer_flush), &
                  new_unittest("test_flush_large_info", test_flush_large_info), &
                  new_unittest("test_flush_all_levels_all_shapes", test_flush_all_levels_all_shapes), &
                  new_unittest("test_flush_reports_overflow_and_truncation", &
                               test_flush_reports_overflow_and_truncation), &
                  new_unittest("test_flush_unknown_level", test_flush_unknown_level), &
                  new_unittest("test_buffer_truncation_flag", test_buffer_truncation_flag), &
                  new_unittest("test_pure_context", test_pure_context) &
                  ]
   end subroutine collect_pic_pure_logger_tests

   subroutine test_buffer_starts_empty(error)
      !! Test that a freshly declared buffer is empty
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf

      call check(error, buf%count == 0, "Buffer count should start at 0")
      if (allocated(error)) return

      call check(error, buf%overflow == 0, "Buffer overflow should start at 0")
      if (allocated(error)) return

      call check(error, buf%truncated == 0, "Buffer truncated should start at 0")
      if (allocated(error)) return
   end subroutine test_buffer_starts_empty

   subroutine test_buffer_single_message(error)
      !! Test buffering a single message
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf

      call pure_info(buf, "Hello from pure land")

      call check(error, buf%count == 1, "Buffer should have 1 entry")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%level) == "INFO", "Level should be INFO")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%message) == "Hello from pure land", &
                 "Message should match")
      if (allocated(error)) return

      call check(error,.not. buf%entries(1)%has_module, "Should not have module")
      if (allocated(error)) return

      call check(error,.not. buf%entries(1)%has_procedure, "Should not have procedure")
      if (allocated(error)) return
   end subroutine test_buffer_single_message

   subroutine test_buffer_all_levels(error)
      !! Test that all convenience methods buffer correctly
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf

      call pure_debug(buf, "debug msg")
      call pure_verbose(buf, "verbose msg")
      call pure_large_info(buf, "large info msg")
      call pure_info(buf, "info msg")
      call pure_performance(buf, "perf msg")
      call pure_warning(buf, "warn msg")
      call pure_error(buf, "error msg")
      call pure_knowledge(buf, "lore msg")

      call check(error, buf%count == 8, "Should have 8 entries")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%level) == "DEBUG", "Entry 1 should be DEBUG")
      if (allocated(error)) return

      call check(error, trim(buf%entries(2)%level) == "VERBOSE", "Entry 2 should be VERBOSE")
      if (allocated(error)) return

      call check(error, trim(buf%entries(3)%level) == "LARGE_INFO", "Entry 3 should be LARGE_INFO")
      if (allocated(error)) return

      call check(error, trim(buf%entries(4)%level) == "INFO", "Entry 4 should be INFO")
      if (allocated(error)) return

      call check(error, trim(buf%entries(5)%level) == "PERFORMANCE", "Entry 5 should be PERFORMANCE")
      if (allocated(error)) return

      call check(error, trim(buf%entries(6)%level) == "WARNING", "Entry 6 should be WARNING")
      if (allocated(error)) return

      call check(error, trim(buf%entries(7)%level) == "ERROR", "Entry 7 should be ERROR")
      if (allocated(error)) return

      call check(error, trim(buf%entries(8)%level) == "LORE", "Entry 8 should be LORE")
      if (allocated(error)) return

      call check(error, trim(buf%entries(3)%message) == "large info msg", &
                 "Large info message should match")
      if (allocated(error)) return
   end subroutine test_buffer_all_levels

   subroutine test_buffer_with_module_procedure(error)
      !! Test buffering with optional module and procedure names
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf

      ! With module only
      call pure_info(buf, "msg with mod", module="my_module")

      call check(error, buf%entries(1)%has_module, "Should have module")
      if (allocated(error)) return

      call check(error,.not. buf%entries(1)%has_procedure, "Should not have procedure")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%module_name) == "my_module", "Module name should match")
      if (allocated(error)) return

      ! With both module and procedure
      call pure_warning(buf, "msg with both", module="my_module", procedure="my_proc")

      call check(error, buf%entries(2)%has_module, "Should have module")
      if (allocated(error)) return

      call check(error, buf%entries(2)%has_procedure, "Should have procedure")
      if (allocated(error)) return

      call check(error, trim(buf%entries(2)%procedure_name) == "my_proc", "Procedure name should match")
      if (allocated(error)) return
   end subroutine test_buffer_with_module_procedure

   subroutine test_buffer_overflow(error)
      !! Test that the buffer handles overflow gracefully
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      integer(default_int) :: i

      ! Fill the buffer completely
      do i = 1, max_log_entries
         call pure_info(buf, "filling up")
      end do

      call check(error, buf%count == max_log_entries, "Buffer should be full")
      if (allocated(error)) return

      call check(error, buf%overflow == 0, "No overflow yet")
      if (allocated(error)) return

      ! Now overflow
      call pure_info(buf, "one too many")
      call pure_info(buf, "two too many")

      call check(error, buf%count == max_log_entries, "Count should not exceed max")
      if (allocated(error)) return

      call check(error, buf%overflow == 2, "Should have 2 overflows")
      if (allocated(error)) return
   end subroutine test_buffer_overflow

   subroutine test_buffer_clear(error)
      !! Test clearing the buffer
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf

      call pure_info(buf, "message 1")
      call pure_debug(buf, "message 2")
      call pure_warning(buf, "message 3")

      call check(error, buf%count == 3, "Should have 3 entries before clear")
      if (allocated(error)) return

      call clear_log_buffer(buf)

      call check(error, buf%count == 0, "Count should be 0 after clear")
      if (allocated(error)) return

      call check(error, buf%overflow == 0, "Overflow should be 0 after clear")
      if (allocated(error)) return

      call check(error, buf%truncated == 0, "Truncated should be 0 after clear")
      if (allocated(error)) return
   end subroutine test_buffer_clear

   subroutine test_buffer_flush(error)
      !! Test flushing the buffer through a real logger
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      type(logger_type) :: logger

      ! Configure logger to debug so everything goes through
      call logger%configure(debug_level)

      call pure_info(buf, "flushed info", module="test_mod")
      call pure_debug(buf, "flushed debug")
      call pure_error(buf, "flushed error", module="test_mod", procedure="test_proc")

      call check(error, buf%count == 3, "Should have 3 entries before flush")
      if (allocated(error)) return

      ! Flush — this is impure, sends to the real logger
      call flush_log_buffer(logger, buf)

      ! Buffer should be cleared after flush
      call check(error, buf%count == 0, "Count should be 0 after flush")
      if (allocated(error)) return
   end subroutine test_buffer_flush

   subroutine test_flush_large_info(error)
      !! Test that buffered LARGE_INFO entries are dispatched through the
      !! logger in all three shapes: bare, module only, and module+procedure
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      type(logger_type) :: logger
      character(*), parameter :: test_filename = "test_pure_large_info.log"
      character(len=256) :: line1, line2, line3
      logical :: file_exists
      integer(default_int) :: ios

      ! Keep the console quiet, capture the dispatched lines in a file instead
      call logger%configure(error_level)
      call logger%set_explicit_printing(.true.)
      call logger%configure_file_output(test_filename, large_info_level)

      call pure_large_info(buf, "bare large info")
      call pure_large_info(buf, "large info with mod", module="pure_mod")
      call pure_large_info(buf, "large info with both", module="pure_mod", procedure="pure_proc")

      call check(error, buf%count == 3, "Should have 3 large_info entries before flush")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%level) == "LARGE_INFO", "Level should be LARGE_INFO")
      if (allocated(error)) return

      call flush_log_buffer(logger, buf)
      call logger%close_log_file()

      call check(error, buf%count == 0, "Count should be 0 after flush")
      if (allocated(error)) return

      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should be created")
      if (allocated(error)) return

      line1 = ""
      line2 = ""
      line3 = ""
      open (unit=logfile_unit, file=test_filename, status="old", action="readwrite", iostat=ios)
      call check(error, ios == 0, "Log file should be readable")
      if (allocated(error)) return

      read (logfile_unit, "(A)", iostat=ios) line1
      read (logfile_unit, "(A)", iostat=ios) line2
      read (logfile_unit, "(A)", iostat=ios) line3
      close (logfile_unit, status="delete")

      call check(error, trim(line1) == "LARGE_INFO: bare large info", &
                 "Bare large_info line should match, got: "//trim(line1))
      if (allocated(error)) return

      call check(error, trim(line2) == "LARGE_INFO: pure_mod: large info with mod", &
                 "Module-only large_info line should match, got: "//trim(line2))
      if (allocated(error)) return

      call check(error, trim(line3) == "LARGE_INFO: pure_mod.pure_proc: large info with both", &
                 "Module+procedure large_info line should match, got: "//trim(line3))
      if (allocated(error)) return

   end subroutine test_flush_large_info

   subroutine test_flush_all_levels_all_shapes(error)
      !! Exercise every dispatch branch: each level buffered bare,
      !! with a module, and with module + procedure
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      type(logger_type) :: logger
      character(*), parameter :: test_filename = "test_pure_dispatch.log"
      character(len=256) :: line
      integer(default_int) :: ios, line_count
      logical :: file_exists

      call logger%configure(error_level)
      call logger%configure_file_output(test_filename, debug_level)

      ! Bare
      call pure_debug(buf, "d")
      call pure_verbose(buf, "v")
      call pure_large_info(buf, "li")
      call pure_info(buf, "i")
      call pure_performance(buf, "p")
      call pure_warning(buf, "w")
      call pure_error(buf, "e")
      call pure_knowledge(buf, "k")

      ! With module
      call pure_debug(buf, "d", module="m")
      call pure_verbose(buf, "v", module="m")
      call pure_large_info(buf, "li", module="m")
      call pure_info(buf, "i", module="m")
      call pure_performance(buf, "p", module="m")
      call pure_warning(buf, "w", module="m")
      call pure_error(buf, "e", module="m")
      call pure_knowledge(buf, "k", module="m")

      ! With module and procedure
      call pure_debug(buf, "d", module="m", procedure="pr")
      call pure_verbose(buf, "v", module="m", procedure="pr")
      call pure_large_info(buf, "li", module="m", procedure="pr")
      call pure_info(buf, "i", module="m", procedure="pr")
      call pure_performance(buf, "p", module="m", procedure="pr")
      call pure_warning(buf, "w", module="m", procedure="pr")
      call pure_error(buf, "e", module="m", procedure="pr")
      call pure_knowledge(buf, "k", module="m", procedure="pr")

      call check(error, buf%count == 24, "Should have 24 entries before flush")
      if (allocated(error)) return

      call flush_log_buffer(logger, buf)
      call logger%close_log_file()

      call check(error, buf%count == 0, "Count should be 0 after flush")
      if (allocated(error)) return

      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should be created")
      if (allocated(error)) return

      ! Every entry should have made it to the file, none swallowed by dispatch
      line_count = 0
      open (unit=logfile_unit, file=test_filename, status="old", action="readwrite", iostat=ios)
      do
         read (logfile_unit, "(A)", iostat=ios) line
         if (ios /= 0) exit
         line_count = line_count + 1
      end do
      close (logfile_unit, status="delete")

      call check(error, line_count == 24, "All 24 entries should have been written to the log file")
      if (allocated(error)) return

   end subroutine test_flush_all_levels_all_shapes

   subroutine test_flush_reports_overflow_and_truncation(error)
      !! Flushing a buffer that overflowed and truncated should emit
      !! the two diagnostic warnings on top of the buffered entries
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      type(logger_type) :: logger
      character(*), parameter :: test_filename = "test_pure_overflow.log"
      character(len=512) :: line
      character(len=300) :: long_message
      integer(default_int) :: i, ios, line_count
      logical :: file_exists

      long_message = repeat("A", 300)

      call logger%configure(error_level)
      call logger%configure_file_output(test_filename, debug_level)

      ! One truncated entry, then fill and overflow the buffer
      call pure_info(buf, long_message)
      do i = 2, max_log_entries
         call pure_info(buf, "filler")
      end do
      call pure_info(buf, "dropped 1")
      call pure_info(buf, "dropped 2")

      call check(error, buf%truncated == 1, "Should have tracked 1 truncation")
      if (allocated(error)) return

      call check(error, buf%overflow == 2, "Should have tracked 2 overflows")
      if (allocated(error)) return

      call flush_log_buffer(logger, buf)
      call logger%close_log_file()

      call check(error, buf%count == 0, "Count should be 0 after flush")
      if (allocated(error)) return

      call check(error, buf%overflow == 0, "Overflow should be reset after flush")
      if (allocated(error)) return

      call check(error, buf%truncated == 0, "Truncated should be reset after flush")
      if (allocated(error)) return

      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should be created")
      if (allocated(error)) return

      ! max_log_entries entries plus a truncation warning and an overflow warning
      line_count = 0
      open (unit=logfile_unit, file=test_filename, status="old", action="readwrite", iostat=ios)
      do
         read (logfile_unit, "(A)", iostat=ios) line
         if (ios /= 0) exit
         line_count = line_count + 1
      end do
      close (logfile_unit, status="delete")

      call check(error, line_count == max_log_entries + 2, &
                 "Flush should log every entry plus the truncation and overflow warnings")
      if (allocated(error)) return

   end subroutine test_flush_reports_overflow_and_truncation

   subroutine test_flush_unknown_level(error)
      !! An entry carrying a level the dispatcher does not know about must
      !! still be logged (as a warning) rather than silently dropped
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      type(logger_type) :: logger
      character(*), parameter :: test_filename = "test_pure_unknown.log"
      character(len=256) :: line
      integer(default_int) :: ios, line_count
      logical :: file_exists

      call logger%configure(error_level)
      call logger%configure_file_output(test_filename, debug_level)

      ! Forge three entries with a level no dispatch branch matches
      call pure_info(buf, "bogus bare")
      call pure_info(buf, "bogus with mod", module="m")
      call pure_info(buf, "bogus with both", module="m", procedure="pr")
      buf%entries(1)%level = "NOT_A_LEVEL"
      buf%entries(2)%level = "NOT_A_LEVEL"
      buf%entries(3)%level = "NOT_A_LEVEL"

      call flush_log_buffer(logger, buf)
      call logger%close_log_file()

      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should be created")
      if (allocated(error)) return

      line_count = 0
      open (unit=logfile_unit, file=test_filename, status="old", action="readwrite", iostat=ios)
      do
         read (logfile_unit, "(A)", iostat=ios) line
         if (ios /= 0) exit
         line_count = line_count + 1
      end do
      close (logfile_unit, status="delete")

      call check(error, line_count == 3, "Unknown-level entries should still be logged")
      if (allocated(error)) return

   end subroutine test_flush_unknown_level

   subroutine test_buffer_truncation_flag(error)
      !! Test that long messages get flagged as truncated
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      character(len=300) :: long_message

      ! Build a message longer than max_message_len (256)
      long_message = repeat("A", 300)

      call pure_info(buf, long_message)

      call check(error, buf%count == 1, "Should have 1 entry")
      if (allocated(error)) return

      call check(error, buf%entries(1)%truncated, "Entry should be flagged as truncated")
      if (allocated(error)) return

      call check(error, buf%truncated == 1, "Buffer should track 1 truncation")
      if (allocated(error)) return

      ! A normal-length message should not be flagged
      call pure_info(buf, "short message")

      call check(error,.not. buf%entries(2)%truncated, "Short message should not be truncated")
      if (allocated(error)) return

      call check(error, buf%truncated == 1, "Still only 1 truncation")
      if (allocated(error)) return
   end subroutine test_buffer_truncation_flag

   subroutine test_pure_context(error)
      !! Test that the pure logger actually works from a pure subroutine
      type(error_type), allocatable, intent(out) :: error
      type(log_buffer_type) :: buf
      integer(default_int), parameter :: test = 42

      call example_pure_routine(test, buf)

      call check(error, buf%count == 2, "Pure routine should have buffered 2 messages")
      if (allocated(error)) return

      call check(error, trim(buf%entries(1)%level) == "INFO", "First should be INFO")
      if (allocated(error)) return

      call check(error, trim(buf%entries(2)%level) == "DEBUG", "Second should be DEBUG")
      if (allocated(error)) return
   end subroutine test_pure_context

   pure subroutine example_pure_routine(x, buf)
      !! A truly pure subroutine that logs via the buffer
      integer(default_int), intent(in) :: x
      type(log_buffer_type), intent(inout) :: buf

      call pure_info(buf, "Entering pure routine", module="test_pure_logger", procedure="example_pure_routine")

      if (x > 0) then
         call pure_debug(buf, "x is positive", module="test_pure_logger", procedure="example_pure_routine")
      end if
   end subroutine example_pure_routine

end module test_pic_pure_logger
