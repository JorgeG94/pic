! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
program term_keys
   !! Manual demo: echo decoded key events until Ctrl-C or q.
   !!
   !! Not a test. It needs a real terminal, which CI does not have, and what
   !! it demonstrates -- that the arrow keys on *your* terminal decode the way
   !! pic thinks they do -- is exactly what an automated test cannot tell you.
   !!
   !! Build with -DPIC_ENABLE_TERM=ON, then run it and press some keys.
   use pic_types, only: default_int, int64
   use pic_error, only: error_t
   use pic_ansi, only: key_event_t, pending_t, decode_keys, &
                       KEY_CHAR, KEY_ENTER, KEY_BACKSPACE, KEY_TAB, KEY_ESC, &
                       KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT, &
                       KEY_HOME, KEY_END, KEY_DELETE, KEY_CTRL_C
   use pic_term, only: TERM_STDIN, term_is_tty, term_enable_vt, &
                       term_raw_enter, term_raw_leave, term_read, term_write
   implicit none

   integer(default_int), parameter :: MAX_EVENTS = 32_default_int
      !! Events decoded from one read. A 64-byte read cannot produce more
      !! than 64, and in practice produces one or two.

   type(error_t) :: err
   type(pending_t) :: pending
   type(key_event_t) :: events(MAX_EVENTS)
   character(len=64) :: buf
   integer(default_int) :: n_events, n_read, i
   logical :: running

   if (.not. term_is_tty(TERM_STDIN)) then
      write (*, "(a)") "term_keys needs a terminal; standard input is not one."
      stop 0
   end if

   call term_enable_vt(err)
   call term_raw_enter(err)
   if (err%has_error()) then
      call err%print_trace()
      stop 1
   end if

   call term_write("Press keys. q or Ctrl-C to quit."//new_line("a")//achar(13))

   running = .true.
   do while (running)
      call term_read(buf, n_read, 200_default_int, err)
      if (err%has_error()) exit
      call decode_keys(pending, buf(1:n_read), events, n_events)
      do i = 1_default_int, n_events
         call term_write(describe(events(i))//new_line("a")//achar(13))
         if (events(i)%code == KEY_CTRL_C) running = .false.
         if (events(i)%code == KEY_CHAR .and. events(i)%char_code == iachar("q")) then
            running = .false.
         end if
      end do
   end do

   ! Idempotent, but called explicitly rather than left to the atexit handler,
   ! because a clean exit should not depend on the crash path.
   call term_raw_leave()
   call term_write("bye"//new_line("a")//achar(13))

contains

   function describe(event) result(text)
      !! One line naming the key.
      type(key_event_t), intent(in) :: event
      character(len=:), allocatable :: text

      select case (event%code)
      case (KEY_CHAR)
         text = "char '"//achar(event%char_code)//"'"
      case (KEY_ENTER)
         text = "Enter"
      case (KEY_BACKSPACE)
         text = "Backspace"
      case (KEY_TAB)
         text = "Tab"
      case (KEY_ESC)
         text = "Escape"
      case (KEY_UP)
         text = "Up"
      case (KEY_DOWN)
         text = "Down"
      case (KEY_LEFT)
         text = "Left"
      case (KEY_RIGHT)
         text = "Right"
      case (KEY_HOME)
         text = "Home"
      case (KEY_END)
         text = "End"
      case (KEY_DELETE)
         text = "Delete"
      case (KEY_CTRL_C)
         text = "Ctrl-C"
      case default
         text = "unknown"
      end select
   end function describe

end program term_keys
