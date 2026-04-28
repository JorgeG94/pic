! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! pic_pure_logger.f90 provides a pure-compatible logger for use inside
!! pure functions and subroutines. Since I/O is forbidden in pure contexts,
!! messages are cached into a log_buffer_type that must be flushed by the
!! caller once execution returns to an impure context.
!!
!! Usage:
!!   use pic_pure_logger, only: log_buffer_type, pure_debug, pure_info, &
!!                               pure_warning, pure_error, flush_log_buffer
!!
!!   type(log_buffer_type) :: log_buf
!!   call pure_routine(args, log_buf)
!!   call flush_log_buffer(global_logger, log_buf)
!!
!! The buffer is intentionally kept simple: fixed-size entries with a count.
!! If the buffer fills up, additional messages are silently dropped and
!! the overflow count is incremented so the caller knows messages were lost.

module pic_pure_logger
   !! Pure-compatible deferred logger for PIC.
   !! Messages are buffered and flushed after the pure call chain returns.
   use pic_types, only: default_int
   use pic_logger, only: logger_type, &
                         debug_level, verbose_level, info_level, performance_level, &
                         warning_level, error_level, knowledge_level

   implicit none
   private

   integer(default_int), parameter, public :: max_log_entries = 64
      !! Maximum number of log entries a buffer can hold
   integer(default_int), parameter :: max_message_len = 256
      !! Maximum length of a log message
   integer(default_int), parameter :: max_label_len = 16
      !! Maximum length of a level label
   integer(default_int), parameter :: max_name_len = 64
      !! Maximum length of a module or procedure name

   type, public :: log_entry_type
      !! A single deferred log entry
      character(len=max_message_len) :: message = ' '
      character(len=max_label_len)   :: level = ' '
      character(len=max_name_len)    :: module_name = ' '
      character(len=max_name_len)    :: procedure_name = ' '
      logical :: has_module = .false.
      logical :: has_procedure = .false.
      logical :: truncated = .false.
         !! True if the message or names exceeded their max length
   end type log_entry_type

   type, public :: log_buffer_type
      !! Buffer that accumulates log entries inside pure code.
      !! Pass as intent(inout) through pure call chains, then
      !! flush with flush_log_buffer once back in impure context.
      type(log_entry_type) :: entries(max_log_entries)
      integer(default_int) :: count = 0
         !! Number of entries currently stored
      integer(default_int) :: overflow = 0
         !! Number of entries dropped because the buffer was full
      integer(default_int) :: truncated = 0
         !! Number of entries where message or names were truncated
   end type log_buffer_type

   public :: pure_debug
   public :: pure_verbose
   public :: pure_info
   public :: pure_performance
   public :: pure_warning
   public :: pure_error
   public :: pure_knowledge
   public :: clear_log_buffer
   public :: flush_log_buffer

contains

   pure subroutine buffer_message(buf, level, message, module, procedure)
      !! Core buffering routine — appends a log entry to the buffer.
      !! If the buffer is full, increments the overflow counter.
      !! If a message or name exceeds its max length, it is truncated
      !! and the entry is flagged.
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: level, message
      character(*), intent(in), optional :: module, procedure

      integer(default_int) :: idx
      logical :: was_truncated

      if (buf%count >= max_log_entries) then
         buf%overflow = buf%overflow + 1
         return
      end if

      buf%count = buf%count + 1
      idx = buf%count
      was_truncated = .false.

      buf%entries(idx)%level = level
      buf%entries(idx)%message = message
      if (len(message) > max_message_len) was_truncated = .true.

      if (present(module)) then
         buf%entries(idx)%module_name = module
         buf%entries(idx)%has_module = .true.
         if (len(module) > max_name_len) was_truncated = .true.
      end if

      if (present(procedure)) then
         buf%entries(idx)%procedure_name = procedure
         buf%entries(idx)%has_procedure = .true.
         if (len(procedure) > max_name_len) was_truncated = .true.
      end if

      if (was_truncated) then
         buf%entries(idx)%truncated = .true.
         buf%truncated = buf%truncated + 1
      end if
   end subroutine buffer_message

   pure subroutine pure_debug(buf, message, module, procedure)
      !! Buffer a debug-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "DEBUG", message, module, procedure)
   end subroutine pure_debug

   pure subroutine pure_verbose(buf, message, module, procedure)
      !! Buffer a verbose-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "VERBOSE", message, module, procedure)
   end subroutine pure_verbose

   pure subroutine pure_info(buf, message, module, procedure)
      !! Buffer an info-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "INFO", message, module, procedure)
   end subroutine pure_info

   pure subroutine pure_performance(buf, message, module, procedure)
      !! Buffer a performance-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "PERFORMANCE", message, module, procedure)
   end subroutine pure_performance

   pure subroutine pure_warning(buf, message, module, procedure)
      !! Buffer a warning-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "WARNING", message, module, procedure)
   end subroutine pure_warning

   pure subroutine pure_error(buf, message, module, procedure)
      !! Buffer an error-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "ERROR", message, module, procedure)
   end subroutine pure_error

   pure subroutine pure_knowledge(buf, message, module, procedure)
      !! Buffer a knowledge-level message
      type(log_buffer_type), intent(inout) :: buf
      character(*), intent(in) :: message
      character(*), intent(in), optional :: module, procedure
      call buffer_message(buf, "LORE", message, module, procedure)
   end subroutine pure_knowledge

   pure subroutine clear_log_buffer(buf)
      !! Reset the buffer to empty. Call after flushing if you want to reuse it.
      type(log_buffer_type), intent(inout) :: buf
      buf%count = 0
      buf%overflow = 0
      buf%truncated = 0
   end subroutine clear_log_buffer

   subroutine flush_log_buffer(logger, buf)
      !! Flush all buffered entries through the real logger, then clear the buffer.
      !! This is impure — call it once you are back outside the pure call chain.
      !!
      !! Usage:
      !!   call flush_log_buffer(global_logger, log_buf)
      !!
      type(logger_type), intent(in) :: logger
      type(log_buffer_type), intent(inout) :: buf

      integer(default_int) :: i

      do i = 1, buf%count
         call dispatch(logger, buf%entries(i))
      end do

      if (buf%truncated > 0) then
         call logger%warning("Pure log buffer: some messages were truncated due to length limits")
      end if

      if (buf%overflow > 0) then
         call logger%warning("Pure log buffer overflow: some messages were dropped")
      end if

      call clear_log_buffer(buf)
   end subroutine flush_log_buffer

   subroutine dispatch(logger, entry)
      !! Route a buffered entry to the appropriate logger method
      type(logger_type), intent(in) :: logger
      type(log_entry_type), intent(in) :: entry

      if (entry%has_module .and. entry%has_procedure) then
         select case (trim(entry%level))
         case ("DEBUG")
            call logger%debug(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("VERBOSE")
            call logger%verbose(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("INFO")
            call logger%info(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("PERFORMANCE")
            call logger%performance(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("WARNING")
            call logger%warning(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("ERROR")
            call logger%error(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case ("LORE")
            call logger%knowledge(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         case default
            call logger%warning(trim(entry%message), trim(entry%module_name), trim(entry%procedure_name))
         end select
      else if (entry%has_module) then
         select case (trim(entry%level))
         case ("DEBUG")
            call logger%debug(trim(entry%message), trim(entry%module_name))
         case ("VERBOSE")
            call logger%verbose(trim(entry%message), trim(entry%module_name))
         case ("INFO")
            call logger%info(trim(entry%message), trim(entry%module_name))
         case ("PERFORMANCE")
            call logger%performance(trim(entry%message), trim(entry%module_name))
         case ("WARNING")
            call logger%warning(trim(entry%message), trim(entry%module_name))
         case ("ERROR")
            call logger%error(trim(entry%message), trim(entry%module_name))
         case ("LORE")
            call logger%knowledge(trim(entry%message), trim(entry%module_name))
         case default
            call logger%warning(trim(entry%message), trim(entry%module_name))
         end select
      else
         select case (trim(entry%level))
         case ("DEBUG")
            call logger%debug(trim(entry%message))
         case ("VERBOSE")
            call logger%verbose(trim(entry%message))
         case ("INFO")
            call logger%info(trim(entry%message))
         case ("PERFORMANCE")
            call logger%performance(trim(entry%message))
         case ("WARNING")
            call logger%warning(trim(entry%message))
         case ("ERROR")
            call logger%error(trim(entry%message))
         case ("LORE")
            call logger%knowledge(trim(entry%message))
         case default
            call logger%warning(trim(entry%message))
         end select
      end if
   end subroutine dispatch

end module pic_pure_logger
