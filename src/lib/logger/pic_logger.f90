!! the pic_logger.f90 is the base file that defines the logger function
!! this is heavily inspired by the logger from the standard library
!! but with some key changes for my purposes.
!! The logger will be the way in which the code interacts
!! with the output to console and files

module pic_logger
!! this is the logger module
   use pic_types, only: default_int
   use pic_global_definitions, only: stdout, logfile_unit

   implicit none
   private
   public :: global_logger, logger_type

   character(*), parameter :: name = "pic_logger"
   integer(default_int), parameter, public :: &
      debug_level = 10, &
      verbose_level = 9, &
      info_level = 8, &
      performance_level = 7, &
      warning_level = 6, &
      error_level = 5

   type :: logger_type
    !! custom logger data type

      private

      integer(default_int), public :: log_level = info_level
        !! set default log level to info
      integer(default_int), public :: log_file_level = verbose_level
        !! set default log file log level to verbose
      integer(default_int), private :: log_file_unit = -1
      logical, private :: log_file_open = .false.

   contains

      procedure, public, pass(self) :: configuration
      procedure, public, pass(self) :: configure
      procedure, public, pass(self) :: configure_file_output
      procedure, public, pass(self) :: close_log_file
      procedure, public, pass(self) :: log
      procedure, public, pass(self) :: debug
      procedure, public, pass(self) :: verbose
      procedure, public, pass(self) :: info
      procedure, public, pass(self) :: performance
      procedure, public, pass(self) :: warning
      procedure, public, pass(self) :: error

   end type logger_type

   type(logger_type) :: global_logger

contains

   pure subroutine configuration(self, level)
      !! Get the current log level set for the logger
      class(logger_type), intent(in) :: self
      integer(default_int), intent(out), optional :: level
      if (present(level)) level = self%log_level
   end subroutine configuration

   pure subroutine configure(self, level)
      !! set the log level for the logger
      class(logger_type), intent(inout) :: self
      integer(default_int), intent(in), optional :: level
      if (present(level)) self%log_level = level
   end subroutine configure

   subroutine configure_file_output(self, filename, level)
      !! configure the log level for the file output
      class(logger_type), intent(inout) :: self
      character(*), intent(in) :: filename
      integer(default_int), intent(in), optional :: level

      integer(default_int) :: ios

      if (self%log_file_open) call self%close_log_file()

      open (unit=logfile_unit, file=trim(filename), status="replace", action="write", iostat=ios)
      if (ios /= 0) then
         write (*, *) "ERROR: Failed to open log file: ", trim(filename)
         return
      end if

      self%log_file_unit = logfile_unit
      self%log_file_open = .true.
      if (present(level)) self%log_file_level = level
   end subroutine configure_file_output

   subroutine close_log_file(self)
      !! simple file closer
      class(logger_type), intent(inout) :: self
      if (self%log_file_open) then
         close (self%log_file_unit)
         self%log_file_open = .false.
         self%log_file_unit = -1
      end if
   end subroutine close_log_file

   subroutine debug(self, message, module, procedure)
      !! log message to debug level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("DEBUG", message, module, procedure)
   end subroutine debug

   subroutine verbose(self, message, module, procedure)
      !! log message to verbose level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("VERBOSE", message, module, procedure)
   end subroutine verbose

   subroutine info(self, message, module, procedure)
      !! log message to info level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("INFO", message, module, procedure)
   end subroutine info

   subroutine warning(self, message, module, procedure)
      !! log message to warning level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("WARNING", message, module, procedure)
   end subroutine warning

   subroutine performance(self, message, module, procedure)
      !! log message to performance level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("PERFORMANCE", message, module, procedure)
   end subroutine performance

   subroutine error(self, message, module, procedure)
      !! log message to error level
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call self%log("ERROR", message, module, procedure)
   end subroutine error

   subroutine write_log_line(unit, level, message, module, procedure)
      !! write the message at the appropriate level with formatting to the string
      integer(default_int), intent(in) :: unit
      character(*), intent(in) :: level, message
      character(*), intent(in), optional :: module, procedure

      if (present(module) .and. present(procedure)) then
         write (unit, '(A, ": ", A, ".", A, ": ", A)') trim(level), trim(module), trim(procedure), trim(message)
      else if (present(module)) then
         write (unit, '(A, ": ", A, ": ", A)') trim(level), trim(module), trim(message)
      else
         write (unit, '(A, ": ", A)') trim(level), trim(message)
      end if
   end subroutine write_log_line

   subroutine log(self, level, message, module, procedure)
      !! general subroutine to control at what level somethings gets printed
      class(logger_type), intent(in) :: self
      character(*), intent(in) :: level
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure

      integer(default_int) :: log_level_value

      select case (trim(level))
      case ("DEBUG")
         log_level_value = debug_level
      case ("VERBOSE")
         log_level_value = verbose_level
      case ("INFO")
         log_level_value = info_level
      case ("WARNING")
         log_level_value = warning_level
      case ("PERFORMANCE")
         log_level_value = performance_level
      case ("ERROR")
         log_level_value = error_level
      case default
         write (*, *) 'ERROR: Invalid log level "', trim(level), '"'
         return
      end select

      ! Console logging
      if (self%log_level >= log_level_value) then
         call write_log_line(stdout, level, message, module, procedure)
      end if

      ! File logging
      if (self%log_file_open .and. self%log_file_level >= log_level_value) then
         call write_log_line(self%log_file_unit, level, message, module, procedure)
      end if

   end subroutine log

end module pic_logger
