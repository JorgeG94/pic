module test_pic_logger
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_logger, only: logger_type, debug_level, verbose_level, large_info_level, &
                         info_level, performance_level, warning_level, error_level, knowledge_level
   use pic_types, only: default_int
   use pic_global_definitions, only: logfile_unit
   implicit none
   private
   public :: collect_pic_logger_tests

contains

   subroutine collect_pic_logger_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_logger_configuration", test_logger_configuration), &
                  new_unittest("test_logger_configure", test_logger_configure), &
                  new_unittest("test_logger_file_output", test_logger_file_output), &
                  new_unittest("test_logger_log_levels", test_logger_log_levels), &
                  new_unittest("test_logger_file_levels", test_logger_file_levels), &
                  new_unittest("test_logger_close_file", test_logger_close_file), &
                  new_unittest("test_logger_convenience_methods", test_logger_convenience_methods), &
                  new_unittest("test_logger_file_content", test_logger_file_content), &
                  new_unittest("test_logger_explicit_printing", test_logger_explicit_printing), &
                  new_unittest("test_logger_reconfigure_and_failed_open", test_logger_reconfigure_and_failed_open), &
                  new_unittest("test_logger_sourced_copy", test_logger_sourced_copy) &
                  ]
   end subroutine collect_pic_logger_tests

   subroutine test_logger_configuration(error)
      !! Test getting logger configuration
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      integer(default_int) :: level

      ! Test default configuration
      call logger%configuration(level)
      call check(error, level == info_level, "Default log level should be info_level")
      if (allocated(error)) return

      ! Test after changing level
      call logger%configure(warning_level)
      call logger%configuration(level)
      call check(error, level == warning_level, "Log level should be warning_level after configure")
      if (allocated(error)) return

   end subroutine test_logger_configuration

   subroutine test_logger_configure(error)
      !! Test configuring logger levels
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      integer(default_int) :: level

      ! Test setting debug level
      call logger%configure(debug_level)
      call logger%configuration(level)
      call check(error, level == debug_level, "Should set debug level")
      if (allocated(error)) return

      ! Test setting error level
      call logger%configure(error_level)
      call logger%configuration(level)
      call check(error, level == error_level, "Should set error level")
      if (allocated(error)) return

   end subroutine test_logger_configure

   subroutine test_logger_file_output(error)
      !! Test file output configuration
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger.log"
      logical :: file_exists

      ! Configure file output
      call logger%configure_file_output(test_filename, verbose_level)

      ! Write something to the file
      call logger%verbose("Test message", "test_module", "test_procedure")

      ! Close the file
      call logger%close_log_file()

      ! Check if file was created
      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should be created")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="readwrite")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_file_output

   subroutine test_logger_log_levels(error)
      !! Test log level filtering logic
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger

      ! Set log level to warning (6)
      call logger%configure(warning_level)

      ! Test that the levels are properly ordered
      call check(error, debug_level > verbose_level, "Debug level should be higher than verbose")
      if (allocated(error)) return

      call check(error, verbose_level > large_info_level, "Verbose level should be higher than large_info")
      if (allocated(error)) return

      call check(error, large_info_level > info_level, "Large_info level should be higher than info")
      if (allocated(error)) return

      call check(error, info_level > performance_level, "Info level should be higher than performance")
      if (allocated(error)) return

      call check(error, performance_level > warning_level, "Performance level should be higher than warning")
      if (allocated(error)) return

      call check(error, warning_level > error_level, "Warning level should be higher than error")
      if (allocated(error)) return

   end subroutine test_logger_log_levels

   subroutine test_logger_file_levels(error)
      !! Test file-specific log levels
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger_levels.log"
      logical :: file_exists

      ! Configure file output with debug level
      call logger%configure_file_output(test_filename, debug_level)

      ! Configure console to error level only
      call logger%configure(error_level)

      ! Write messages at different levels
      call logger%debug("Debug message", "test_module")
      call logger%info("Info message", "test_module")
      call logger%error("Error message", "test_module")

      ! Close file
      call logger%close_log_file()

      ! Check if file exists
      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should exist")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_file_levels

   subroutine test_logger_close_file(error)
      !! Test closing log file
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger_close.log"
      logical :: file_exists

      ! Configure file output
      call logger%configure_file_output(test_filename)

      ! Write a message
      call logger%info("Test message")

      ! Close file
      call logger%close_log_file()

      ! Try to close again (should not cause error)
      call logger%close_log_file()

      ! Check if file exists
      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should exist after closing")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_close_file

   subroutine test_logger_convenience_methods(error)
      !! Test all convenience methods (debug, verbose, info, etc.)
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger_methods.log"
      logical :: file_exists

      ! Configure file output to capture all messages
      call logger%configure_file_output(test_filename, debug_level)
      call logger%configure(debug_level)

      ! Test all convenience methods
      call logger%debug("Debug message")
      call logger%verbose("Verbose message")
      call logger%large_info("Large info message")
      call logger%info("Info message")
      call logger%performance("Performance message")
      call logger%warning("Warning message")
      call logger%error("Error message")
      call logger%knowledge("LORE")

      ! Test with module and procedure
      call logger%info("Test message", "test_module", "test_procedure")
      call logger%warning("Test message", "test_module")

      ! Close file
      call logger%close_log_file()

      ! Check if file exists
      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should exist")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_convenience_methods

   subroutine test_logger_file_content(error)
      !! Test actual file content
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger_content.log"
      character(len=200) :: line
      logical :: file_exists, found_message
      integer(default_int) :: ios, unit_num

      ! Configure file output
      call logger%configure_file_output(test_filename, info_level)

      ! Write a specific message
      call logger%info("Test content message", "test_module", "test_procedure")

      ! Close file
      call logger%close_log_file()

      ! Check if file exists
      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should exist")
      if (allocated(error)) return

      ! Read file content
      found_message = .false.
      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read
         if (index(line, "Test content message") > 0 .and. &
             index(line, "test_module") > 0 .and. &
             index(line, "test_procedure") > 0 .and. &
             index(line, "INFO") == 0) then
            found_message = .true.
            exit read
         end if
      end do read
      close (unit_num)

      call check(error, found_message, "Should find expected message format in log file without level prefix")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_file_content

   subroutine test_logger_explicit_printing(error)
      !! Test explicit level-prefix printing toggle
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: test_filename = "test_logger_explicit.log"
      character(len=200) :: line
      logical :: file_exists
      logical :: found_plain_message
      logical :: found_plain_module_message
      logical :: found_plain_module_procedure_message
      logical :: found_explicit_message
      logical :: found_explicit_module_message
      logical :: found_explicit_module_procedure_message
      integer(default_int) :: ios, unit_num

      call logger%configure_file_output(test_filename, info_level)

      ! Default behavior should hide level prefixes
      call logger%info("Plain message")
      call logger%info("Plain module message", "test_module")
      call logger%info("Plain module.procedure message", "test_module", "test_procedure")

      ! Explicit mode should include level prefixes
      call logger%set_explicit_printing(.true.)
      call logger%info("Explicit message")
      call logger%info("Explicit module message", "test_module")
      call logger%info("Explicit module.procedure message", "test_module", "test_procedure")
      call logger%close_log_file()

      inquire (file=test_filename, exist=file_exists)
      call check(error, file_exists, "Log file should exist")
      if (allocated(error)) return

      found_plain_message = .false.
      found_plain_module_message = .false.
      found_plain_module_procedure_message = .false.
      found_explicit_message = .false.
      found_explicit_module_message = .false.
      found_explicit_module_procedure_message = .false.
      open (newunit=unit_num, file=test_filename, status="old", action="read")
      read: do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit read
         select case (trim(line))
         case ("Plain message")
            found_plain_message = .true.
         case ("test_module: Plain module message")
            found_plain_module_message = .true.
         case ("test_module.test_procedure: Plain module.procedure message")
            found_plain_module_procedure_message = .true.
         case ("INFO: Explicit message")
            found_explicit_message = .true.
         case ("INFO: test_module: Explicit module message")
            found_explicit_module_message = .true.
         case ("INFO: test_module.test_procedure: Explicit module.procedure message")
            found_explicit_module_procedure_message = .true.
         end select
      end do read
      close (unit_num)

      call check(error, found_plain_message, "Default plain message format should be logged without level prefix")
      if (allocated(error)) return
      call check(error, found_plain_module_message, "Default plain module format should be logged without level prefix")
      if (allocated(error)) return
      call check(error, found_plain_module_procedure_message, &
                 "Default plain module.procedure format should be logged without level prefix")
      if (allocated(error)) return
      call check(error, found_explicit_message, "Explicit message should include level prefix when enabled")
      if (allocated(error)) return
      call check(error, found_explicit_module_message, &
                 "Explicit module format should include level prefix when enabled")
      if (allocated(error)) return
      call check(error, found_explicit_module_procedure_message, &
                 "Explicit module.procedure format should include level prefix when enabled")
      if (allocated(error)) return

      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if
   end subroutine test_logger_explicit_printing

   subroutine test_logger_reconfigure_and_failed_open(error)
      !! Reconfiguring file output must close the previous file first, and a
      !! file that cannot be opened must leave file logging switched off
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      character(len=*), parameter :: file_a = "test_logger_switch_a.log"
      character(len=*), parameter :: file_b = "test_logger_switch_b.log"
      character(len=*), parameter :: bad_file = "pic_no_such_directory/impossible.log"
      logical :: a_has_a, a_has_b, b_has_a, b_has_b, b_has_c

      call logger%configure_file_output(file_a, info_level)
      call logger%info("alpha entry")

      ! Switching targets has to close file A before opening file B.
      call logger%configure_file_output(file_b, info_level)
      call logger%info("bravo entry")
      call logger%close_log_file()

      call scan_log(file_a, "alpha entry", a_has_a)
      call scan_log(file_a, "bravo entry", a_has_b)
      call scan_log(file_b, "alpha entry", b_has_a)
      call scan_log(file_b, "bravo entry", b_has_b)

      call check(error, a_has_a, "First file should hold the message written while it was open")
      if (allocated(error)) return

      call check(error,.not. a_has_b, "First file must be closed once the target is switched")
      if (allocated(error)) return

      call check(error, b_has_b, "Second file should hold the message written after the switch")
      if (allocated(error)) return

      call check(error,.not. b_has_a, "Second file must not receive earlier messages")
      if (allocated(error)) return

      ! An unopenable path must be reported and must not enable file logging.
      call logger%configure_file_output(bad_file, info_level)
      call logger%info("charlie entry")
      call logger%close_log_file()

      call scan_log(file_b, "charlie entry", b_has_c)

      call check(error,.not. b_has_c, "A failed open must not resume writing to the previous file")
      if (allocated(error)) return

      call delete_log(file_a)
      call delete_log(file_b)
   end subroutine test_logger_reconfigure_and_failed_open

   subroutine test_logger_sourced_copy(error)
      !! A sourced allocation of a logger must carry its configured level over
      type(error_type), allocatable, intent(out) :: error
      type(logger_type) :: logger
      class(logger_type), allocatable :: copy
      integer(default_int) :: level

      call logger%configure(debug_level)

      allocate (copy, source=logger)
      call copy%configuration(level)

      call check(error, level == debug_level, "A copied logger should keep the configured level")
      if (allocated(error)) return

      ! The copy must be independent of the original.
      call copy%configure(error_level)
      call logger%configuration(level)

      call check(error, level == debug_level, "Reconfiguring the copy must not touch the original")
      if (allocated(error)) return
   end subroutine test_logger_sourced_copy

   subroutine scan_log(filename, needle, found)
      !! Report whether 'needle' occurs anywhere in 'filename'
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: needle
      logical, intent(out) :: found
      character(len=512) :: line
      integer(default_int) :: unit_num, ios
      logical :: exists

      found = .false.
      inquire (file=filename, exist=exists)
      if (.not. exists) return

      open (newunit=unit_num, file=filename, status="old", action="read")
      do
         read (unit_num, "(A)", iostat=ios) line
         if (ios /= 0) exit
         if (index(line, needle) > 0) found = .true.
      end do
      close (unit_num)
   end subroutine scan_log

   subroutine delete_log(filename)
      !! Remove a log file if it exists
      character(len=*), intent(in) :: filename
      integer(default_int) :: unit_num
      logical :: exists

      inquire (file=filename, exist=exists)
      if (exists) then
         open (newunit=unit_num, file=filename, status="old", action="read")
         close (unit_num, status="delete")
      end if
   end subroutine delete_log

end module test_pic_logger
