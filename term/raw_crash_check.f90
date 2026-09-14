program raw_crash
   !! Enters raw mode and then dies of error stop. The harness compares
   !! `stty -g` before and after: if the atexit handler did not fire, the
   !! terminal is left raw and the two differ.
   use pic_error, only: error_t
   use pic_term, only: term_raw_enter, term_raw_is_active, term_is_tty, TERM_STDIN
   implicit none
   type(error_t) :: err

   if (.not. term_is_tty(TERM_STDIN)) then
      write (*, "(a)") "NOTTY"
      stop 0
   end if
   call term_raw_enter(err)
   if (err%has_error()) then
      write (*, "(a)") "ENTERFAILED"
      stop 0
   end if
   if (.not. term_raw_is_active()) then
      write (*, "(a)") "NOTACTIVE"
      stop 0
   end if
   write (*, "(a)") "RAWON"
   error stop 3
end program raw_crash
