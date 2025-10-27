module test_pic_logger
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_logger, only: logger_type, debug_level, verbose_level, info_level, &
                         performance_level, warning_level, error_level, knowledge_level
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
                  new_unittest("test_logger_file_content", test_logger_file_content) &
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

      call check(error, verbose_level > info_level, "Verbose level should be higher than info")
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
             index(line, "INFO") > 0) then
            found_message = .true.
            exit read
         end if
      end do read
      close (unit_num)

      call check(error, found_message, "Should find expected message format in log file")
      if (allocated(error)) return

      ! Clean up
      if (file_exists) then
         open (unit=logfile_unit, file=test_filename, status="old", action="read")
         close (logfile_unit, status="delete")
      end if

   end subroutine test_logger_file_content

end module test_pic_logger
