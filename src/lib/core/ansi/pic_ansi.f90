! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! ANSI escape sequences, key decoding and flicker-free frame composition.
module pic_ansi
   !! The half of a terminal interface that is pure string processing.
   !!
   !! Nothing here does any I/O, opens anything, or asks the operating system
   !! a question. Every escape builder is a `pure` function returning a
   !! string, the key decoder is a `pure` subroutine over a byte array, and
   !! the frame composer hands back one string for the caller to write. The
   !! parts that must talk to the OS -- raw mode, terminal size, reading with
   !! a timeout -- live in `pic_term`.
   !!
   !! That split is what makes this testable: every case below is exercised
   !! without a terminal, because there is nothing here that needs one.
   !!
   !! ### Quick start
   !!
   !! ```fortran
   !! use pic_ansi
   !!
   !! type(frame_t) :: frame
   !! type(key_event_t) :: events(16)
   !! integer(default_int) :: n
   !!
   !! write (*, "(a)", advance="no") ansi_alt_screen_enter()//ansi_hide_cursor()
   !!
   !! call frame%resize(24_default_int, 80_default_int)
   !! call frame%set_line(1_default_int, styled("ARRIVALS", fg=ANSI_CYAN, bold=.true.))
   !! write (*, "(a)", advance="no") frame%render()
   !!
   !! call decode_keys(pending, raw_bytes, events, n)
   !! ```
   !!
   !! ### Width is counted in code points
   !!
   !! `set_line` truncates to the frame width in UTF-8 code points, not bytes,
   !! so a row of box-drawing characters -- three bytes each, one column each
   !! -- is not cut a third of the way along. East Asian wide characters, and
   !! combining marks, still count as one column each; getting those right
   !! needs a character-width table and is out of scope here.
   use pic_types, only: default_int, int32
   use pic_strings, only: to_string
   use pic_string_type, only: string_type, char, assignment(=), operator(==)
   implicit none
   private

   public :: ansi_clear_screen, ansi_clear_line, ansi_move_to
   public :: ansi_hide_cursor, ansi_show_cursor
   public :: ansi_alt_screen_enter, ansi_alt_screen_leave
   public :: ansi_fg, ansi_bg, ansi_fg_256, ansi_bg_256, ansi_fg_rgb, ansi_bg_rgb
   public :: ansi_bold, ansi_reverse, ansi_reset
   public :: styled
   public :: key_event_t, decode_keys, pending_t
   public :: frame_t
   public :: display_width, truncate_to_width

   character(len=*), parameter, public :: ESC = achar(27)
      !! The escape character, 0x1B.
   character(len=*), parameter :: CSI = ESC//"["
      !! Control Sequence Introducer.

   integer(int32), parameter, public :: ANSI_BLACK = 0_int32
   integer(int32), parameter, public :: ANSI_RED = 1_int32
   integer(int32), parameter, public :: ANSI_GREEN = 2_int32
   integer(int32), parameter, public :: ANSI_YELLOW = 3_int32
   integer(int32), parameter, public :: ANSI_BLUE = 4_int32
   integer(int32), parameter, public :: ANSI_MAGENTA = 5_int32
   integer(int32), parameter, public :: ANSI_CYAN = 6_int32
   integer(int32), parameter, public :: ANSI_WHITE = 7_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_BLACK = 8_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_RED = 9_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_GREEN = 10_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_YELLOW = 11_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_BLUE = 12_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_MAGENTA = 13_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_CYAN = 14_int32
   integer(int32), parameter, public :: ANSI_BRIGHT_WHITE = 15_int32
      !! The sixteen named colours. 0-7 are the normal set, 8-15 the bright
      !! set, which is why `ansi_fg` splits at 8 rather than taking two
      !! separate entry points.

   integer(int32), parameter, public :: KEY_NONE = 0_int32
      !! No event. A `key_event_t` starts here.
   integer(int32), parameter, public :: KEY_CHAR = 1_int32
      !! A printable byte; `char_code` holds it.
   integer(int32), parameter, public :: KEY_ENTER = 2_int32
   integer(int32), parameter, public :: KEY_BACKSPACE = 3_int32
   integer(int32), parameter, public :: KEY_TAB = 4_int32
   integer(int32), parameter, public :: KEY_ESC = 5_int32
   integer(int32), parameter, public :: KEY_UP = 6_int32
   integer(int32), parameter, public :: KEY_DOWN = 7_int32
   integer(int32), parameter, public :: KEY_RIGHT = 8_int32
   integer(int32), parameter, public :: KEY_LEFT = 9_int32
   integer(int32), parameter, public :: KEY_HOME = 10_int32
   integer(int32), parameter, public :: KEY_END = 11_int32
   integer(int32), parameter, public :: KEY_DELETE = 12_int32
   integer(int32), parameter, public :: KEY_CTRL_C = 13_int32

   integer(default_int), parameter, public :: PENDING_CAPACITY = 16_default_int
      !! Longest partial escape sequence carried between reads. The sequences
      !! recognised here are at most six bytes; the rest is slack so that a
      !! hostile or unknown sequence cannot overrun the buffer.

   type :: pending_t
      !! Bytes of an escape sequence that arrived without their continuation.
      !!
      !! A terminal is free to split `ESC [ A` across two reads, and a decoder
      !! that forgot the `ESC` between calls would report a stray escape and
      !! then the letter A. This carries the fragment across.
      private
      character(len=PENDING_CAPACITY) :: buffer = ""
         !! Fixed store, so a `pending_t` needs no allocation and can be a
         !! plain component of a caller's state.
      integer(default_int) :: length = 0_default_int
         !! Bytes in use.
   contains
      procedure :: clear => pending_clear
      procedure :: is_empty => pending_is_empty
   end type pending_t

   type :: key_event_t
      !! One decoded key press.
      integer(int32) :: code = KEY_NONE
         !! One of the KEY_* codes.
      integer(int32) :: char_code = 0_int32
         !! Byte value, meaningful when `code` is `KEY_CHAR`.
   end type key_event_t

   type :: frame_t
      !! A screen's worth of text lines, rendered by diffing against the last.
      !!
      !! The model is "set every line, render, repeat". `render` compares each
      !! line with what was last rendered and emits moves and rewrites only
      !! for the rows that changed, so an otherwise idle board that updates a
      !! clock writes one line rather than twenty-four. That is what makes the
      !! simple model flicker-free.
      private
      type(string_type), allocatable :: current(:)
         !! Lines as set since the last render.
         !!
         !! `string_type` rather than `character(len=:), allocatable :: (:)`
         !! for two reasons, both learned the hard way.
         !!
         !! A deferred-length allocatable *array* cannot be blanked with a
         !! whole-array `= ""`: F2018 10.2.1.3 deallocates and reallocates an
         !! allocatable whose length type parameter differs from the
         !! expression's, so that assignment sets the length to zero. GNU,
         !! AOCC and LFortran keep the length and blank-pad; Intel and NVIDIA
         !! reallocate. Both readings are defensible and the construct is
         !! therefore unusable here.
         !!
         !! And a fixed row length has to be guessed. Escape sequences make a
         !! styled line far longer in bytes than in columns -- per-character
         !! colouring runs to about ten bytes a column -- so any constant
         !! multiple of the width is a cap waiting to truncate someone's row.
      type(string_type), allocatable :: shown(:)
         !! Lines as of the last render, for diffing.
      integer(default_int) :: rows = 0_default_int
         !! Number of lines.
      integer(default_int) :: cols = 0_default_int
         !! Width in code points; `set_line` truncates to it.
      logical :: full_redraw = .true.
         !! Set by `resize` and `invalidate`; makes the next render emit every
         !! row regardless of the diff.
   contains
      procedure :: resize => frame_resize
      procedure :: set_line => frame_set_line
      procedure :: render => frame_render
      procedure :: invalidate => frame_invalidate
      procedure :: row_count => frame_row_count
      procedure :: col_count => frame_col_count
   end type frame_t

contains

   ! ---- escape builders -----------------------------------------------------

   pure function ansi_clear_screen() result(seq)
      !! Clear the whole screen and home the cursor.
      character(len=:), allocatable :: seq

      seq = CSI//"2J"//CSI//"H"
   end function ansi_clear_screen

   pure function ansi_clear_line() result(seq)
      !! Clear the line the cursor is on.
      character(len=:), allocatable :: seq

      seq = CSI//"2K"
   end function ansi_clear_line

   pure function ansi_move_to(row, col) result(seq)
      !! Move the cursor to a one-based row and column.
      !!
      !! Numbers are converted with `to_string`, never with internal I/O: a
      !! `write` to an internal unit is not allowed in a `pure` procedure, and
      !! it would drag in locale-dependent formatting for no gain.
      integer(default_int), intent(in) :: row
         !! One-based row; values below 1 are clamped to 1.
      integer(default_int), intent(in) :: col
         !! One-based column; values below 1 are clamped to 1.
      character(len=:), allocatable :: seq

      seq = CSI//to_string(int(max(1_default_int, row), int32))//";"// &
            to_string(int(max(1_default_int, col), int32))//"H"
   end function ansi_move_to

   pure function ansi_hide_cursor() result(seq)
      !! Hide the cursor.
      character(len=:), allocatable :: seq

      seq = CSI//"?25l"
   end function ansi_hide_cursor

   pure function ansi_show_cursor() result(seq)
      !! Show the cursor.
      character(len=:), allocatable :: seq

      seq = CSI//"?25h"
   end function ansi_show_cursor

   pure function ansi_alt_screen_enter() result(seq)
      !! Switch to the alternate screen buffer.
      !!
      !! The shell's scrollback is left untouched, so leaving restores exactly
      !! what the user was looking at before the program started.
      character(len=:), allocatable :: seq

      seq = CSI//"?1049h"
   end function ansi_alt_screen_enter

   pure function ansi_alt_screen_leave() result(seq)
      !! Switch back to the normal screen buffer.
      character(len=:), allocatable :: seq

      seq = CSI//"?1049l"
   end function ansi_alt_screen_leave

   pure function ansi_fg(color) result(seq)
      !! Set the foreground to one of the sixteen named colours.
      integer(int32), intent(in) :: color
         !! One of the ANSI_* colour constants, 0 to 15.
      character(len=:), allocatable :: seq

      seq = CSI//to_string(base_code(color, 30_int32))//"m"
   end function ansi_fg

   pure function ansi_bg(color) result(seq)
      !! Set the background to one of the sixteen named colours.
      integer(int32), intent(in) :: color
         !! One of the ANSI_* colour constants, 0 to 15.
      character(len=:), allocatable :: seq

      seq = CSI//to_string(base_code(color, 40_int32))//"m"
   end function ansi_bg

   pure function base_code(color, base) result(code)
      !! SGR number for a named colour: `base + n`, or `base + 60 + n` bright.
      integer(int32), intent(in) :: color
      integer(int32), intent(in) :: base
         !! 30 for foreground, 40 for background.
      integer(int32) :: code

      integer(int32) :: clamped

      clamped = max(0_int32, min(15_int32, color))
      if (clamped < 8_int32) then
         code = base + clamped
      else
         code = base + 60_int32 + (clamped - 8_int32)
      end if
   end function base_code

   pure function ansi_fg_256(index) result(seq)
      !! Set the foreground from the 256-colour cube.
      integer(int32), intent(in) :: index
         !! Palette index, clamped to 0 to 255.
      character(len=:), allocatable :: seq

      seq = CSI//"38;5;"//to_string(clamp_byte(index))//"m"
   end function ansi_fg_256

   pure function ansi_bg_256(index) result(seq)
      !! Set the background from the 256-colour cube.
      integer(int32), intent(in) :: index
         !! Palette index, clamped to 0 to 255.
      character(len=:), allocatable :: seq

      seq = CSI//"48;5;"//to_string(clamp_byte(index))//"m"
   end function ansi_bg_256

   pure function ansi_fg_rgb(r, g, b) result(seq)
      !! Set the foreground to a 24-bit colour.
      integer(int32), intent(in) :: r
         !! Red, clamped to 0 to 255.
      integer(int32), intent(in) :: g
         !! Green, clamped to 0 to 255.
      integer(int32), intent(in) :: b
         !! Blue, clamped to 0 to 255.
      character(len=:), allocatable :: seq

      seq = CSI//"38;2;"//to_string(clamp_byte(r))//";"//to_string(clamp_byte(g))// &
            ";"//to_string(clamp_byte(b))//"m"
   end function ansi_fg_rgb

   pure function ansi_bg_rgb(r, g, b) result(seq)
      !! Set the background to a 24-bit colour.
      integer(int32), intent(in) :: r
         !! Red, clamped to 0 to 255.
      integer(int32), intent(in) :: g
         !! Green, clamped to 0 to 255.
      integer(int32), intent(in) :: b
         !! Blue, clamped to 0 to 255.
      character(len=:), allocatable :: seq

      seq = CSI//"48;2;"//to_string(clamp_byte(r))//";"//to_string(clamp_byte(g))// &
            ";"//to_string(clamp_byte(b))//"m"
   end function ansi_bg_rgb

   pure function clamp_byte(value) result(r)
      !! Clamp to 0 to 255, so a caller's arithmetic slip cannot emit a
      !! sequence the terminal will misparse.
      integer(int32), intent(in) :: value
      integer(int32) :: r

      r = max(0_int32, min(255_int32, value))
   end function clamp_byte

   pure function ansi_bold() result(seq)
      !! Turn on bold.
      character(len=:), allocatable :: seq

      seq = CSI//"1m"
   end function ansi_bold

   pure function ansi_reverse() result(seq)
      !! Swap foreground and background.
      character(len=:), allocatable :: seq

      seq = CSI//"7m"
   end function ansi_reverse

   pure function ansi_reset() result(seq)
      !! Reset every attribute.
      character(len=:), allocatable :: seq

      seq = CSI//"0m"
   end function ansi_reset

   pure function styled(text, fg, bg, bold) result(seq)
      !! Wrap `text` in the requested attributes and a reset.
      !!
      !! The reset is emitted only when something was actually set, so
      !! `styled("x")` is `"x"` and not `"x"` with a stray reset after it.
      character(len=*), intent(in) :: text
         !! Text to wrap.
      integer(int32), intent(in), optional :: fg
         !! Foreground colour, one of the ANSI_* constants.
      integer(int32), intent(in), optional :: bg
         !! Background colour, one of the ANSI_* constants.
      logical, intent(in), optional :: bold
         !! Whether to embolden.
      character(len=:), allocatable :: seq

      logical :: any_set

      seq = ""
      any_set = .false.
      if (present(fg)) then
         seq = seq//ansi_fg(fg)
         any_set = .true.
      end if
      if (present(bg)) then
         seq = seq//ansi_bg(bg)
         any_set = .true.
      end if
      if (present(bold)) then
         if (bold) then
            seq = seq//ansi_bold()
            any_set = .true.
         end if
      end if
      seq = seq//text
      if (any_set) seq = seq//ansi_reset()
   end function styled

   ! ---- key decoding --------------------------------------------------------

   pure subroutine pending_clear(this)
      !! Forget any partial escape sequence.
      class(pending_t), intent(inout) :: this

      this%buffer = ""
      this%length = 0_default_int
   end subroutine pending_clear

   pure function pending_is_empty(this) result(r)
      !! Whether a partial sequence is being carried.
      class(pending_t), intent(in) :: this
      logical :: r

      r = this%length == 0_default_int
   end function pending_is_empty

   pure subroutine decode_keys(pending, bytes, events, n_events)
      !! Decode raw terminal bytes into key events.
      !!
      !! A state machine rather than a lookup, because escape sequences arrive
      !! in whatever chunks the terminal and the kernel decide on: `ESC [ A`
      !! may come as one read or three. Anything that ends mid-sequence is
      !! kept in `pending` and completed by the next call.
      !!
      !! A lone `ESC` -- the user pressing Escape, as opposed to the start of
      !! a sequence -- is indistinguishable from a truncated sequence until
      !! more bytes arrive or do not. It is held in `pending`, and reported as
      !! `KEY_ESC` on the next call if nothing followed it. That is one read's
      !! worth of latency on the Escape key and no ambiguity, which is the
      !! trade every terminal program makes.
      !!
      !! Multi-byte UTF-8 is passed through as individual `KEY_CHAR` bytes; a
      !! caller that wants code points reassembles them.
      type(pending_t), intent(inout) :: pending
         !! Carried partial sequence; pass the same one every call.
      character(len=*), intent(in) :: bytes
         !! Bytes just read from the terminal. May be empty, which is how a
         !! caller flushes a held `ESC`.
      type(key_event_t), intent(out) :: events(:)
         !! Decoded events, filled from 1.
      integer(default_int), intent(out) :: n_events
         !! How many entries of `events` were filled.

      character(len=PENDING_CAPACITY + 1) :: work
      integer(default_int) :: used, i, n, consumed
      type(key_event_t) :: event
      logical :: complete

      n_events = 0_default_int

      ! An empty read with a held ESC means the user really did press Escape:
      ! nothing followed it, so it cannot be the start of a sequence.
      if (len(bytes) == 0) then
         if (pending%length > 0_default_int) then
            if (pending%buffer(1:1) == ESC .and. pending%length == 1_default_int) then
               call emit(events, n_events, KEY_ESC, 0_int32)
            end if
            call pending%clear()
         end if
         return
      end if

      used = pending%length
      work = ""
      if (used > 0_default_int) work(1:used) = pending%buffer(1:used)

      n = len(bytes, kind=default_int)
      i = 1_default_int
      do while (i <= n)
         if (used == 0_default_int .and. bytes(i:i) /= ESC) then
            ! the common case: an ordinary byte, no sequence in progress
            call decode_single(iachar(bytes(i:i), default_int), event)
            call emit(events, n_events, event%code, event%char_code)
            i = i + 1_default_int
            cycle
         end if

         ! inside a sequence: accumulate and test for completeness
         if (used >= PENDING_CAPACITY) then
            ! Not a sequence we know, and too long to be one. Drop it rather
            ! than grow without bound, and resynchronise on the next byte.
            used = 0_default_int
            cycle
         end if
         used = used + 1_default_int
         work(used:used) = bytes(i:i)
         i = i + 1_default_int

         call match_sequence(work(1:used), complete, event, consumed)
         if (complete) then
            if (event%code /= KEY_NONE) call emit(events, n_events, event%code, event%char_code)
            used = used - consumed
            if (used > 0_default_int) work(1:used) = work(consumed + 1:consumed + used)
         end if
      end do

      call pending%clear()
      if (used > 0_default_int) then
         pending%buffer(1:used) = work(1:used)
         pending%length = used
      end if
   end subroutine decode_keys

   pure subroutine emit(events, n_events, code, char_code)
      !! Append an event, silently dropping any beyond the caller's array.
      type(key_event_t), intent(inout) :: events(:)
      integer(default_int), intent(inout) :: n_events
      integer(int32), intent(in) :: code
      integer(int32), intent(in) :: char_code

      if (n_events >= size(events, kind=default_int)) return
      n_events = n_events + 1_default_int
      events(n_events)%code = code
      events(n_events)%char_code = char_code
   end subroutine emit

   pure subroutine decode_single(byte, event)
      !! Decode one byte that is not part of an escape sequence.
      integer(default_int), intent(in) :: byte
         !! `iachar` value of the byte.
      type(key_event_t), intent(out) :: event

      event%char_code = 0_int32
      select case (byte)
      case (13, 10)
         event%code = KEY_ENTER
      case (127, 8)
         event%code = KEY_BACKSPACE
      case (9)
         event%code = KEY_TAB
      case (3)
         event%code = KEY_CTRL_C
      case default
         event%code = KEY_CHAR
         event%char_code = int(byte, int32)
      end select
   end subroutine decode_single

   pure subroutine match_sequence(seq, complete, event, consumed)
      !! Test whether `seq` is a finished escape sequence.
      !!
      !! `complete` false means "could still become one, keep accumulating".
      !! `complete` true with `KEY_NONE` means "definitely not one" and the
      !! bytes are discarded, which is how an unknown sequence resynchronises.
      character(len=*), intent(in) :: seq
         !! Bytes accumulated so far, starting with ESC.
      logical, intent(out) :: complete
         !! Whether a decision has been reached.
      type(key_event_t), intent(out) :: event
         !! The decoded key, or `KEY_NONE` if the sequence is unknown.
      integer(default_int), intent(out) :: consumed
         !! How many leading bytes of `seq` the decision used.

      integer(default_int) :: n

      complete = .false.
      event%code = KEY_NONE
      event%char_code = 0_int32
      consumed = len(seq, kind=default_int)
      n = len(seq, kind=default_int)

      if (n == 1_default_int) return              ! just ESC so far

      ! `ESC [` and `ESC O` both introduce the sequences below; xterm emits
      ! the first in normal mode and the second in application cursor mode,
      ! and a program that handles only one loses the arrow keys in the other.
      if (seq(2:2) /= "[" .and. seq(2:2) /= "O") then
         complete = .true.                        ! ESC followed by something else
         return
      end if
      if (n == 2_default_int) return

      select case (seq(3:3))
      case ("A")
         event%code = KEY_UP
      case ("B")
         event%code = KEY_DOWN
      case ("C")
         event%code = KEY_RIGHT
      case ("D")
         event%code = KEY_LEFT
      case ("H")
         event%code = KEY_HOME
      case ("F")
         event%code = KEY_END
      case ("1", "3", "4", "7", "8")
         ! numeric forms: ESC [ 3 ~ and friends, which need the tilde
         if (n == 3_default_int) return
         if (seq(4:4) /= "~") then
            complete = .true.
            return
         end if
         select case (seq(3:3))
         case ("1", "7")
            event%code = KEY_HOME
         case ("4", "8")
            event%code = KEY_END
         case ("3")
            event%code = KEY_DELETE
         case default
            ! Unreachable: the outer select already narrowed seq(3:3) to these
            ! five. Spelled out so the reader, and the linter, do not have to
            ! hold the outer case list in mind to see that it is total.
            event%code = KEY_NONE
         end select
         complete = .true.
         consumed = 4_default_int
         return
      case default
         complete = .true.                        ! unknown; drop it
         return
      end select

      complete = .true.
      consumed = 3_default_int
   end subroutine match_sequence

   ! ---- width ---------------------------------------------------------------

   pure function escape_length(text, start) result(n)
      !! Length of the escape sequence beginning at `start`, or 0.
      !!
      !! Escape sequences occupy no columns, so measuring and truncating both
      !! have to step over them rather than count them. Without this,
      !! `styled("ARRIVALS", fg=ANSI_RED)` measures seventeen wide rather than
      !! eight, and truncating a styled row could cut an escape sequence in
      !! half -- which does not merely lose the colour, it leaves the terminal
      !! reading the rest of the row as parameters.
      !!
      !! Recognises the CSI form, `ESC [` then parameter and intermediate
      !! bytes then a final byte in the range `@` to `~`, which is what
      !! everything in this module emits. An unterminated sequence swallows
      !! the remaining text, since there is no position in it that could
      !! safely be cut. Any other `ESC` pair counts as two bytes.
      character(len=*), intent(in) :: text
         !! Text being scanned.
      integer(default_int), intent(in) :: start
         !! Index to examine.
      integer(default_int) :: n

      integer(default_int) :: j, last, byte

      n = 0_default_int
      last = len(text, kind=default_int)
      if (start > last) return
      if (text(start:start) /= ESC) return
      if (start == last) then
         n = 1_default_int                 ! a lone trailing ESC
         return
      end if
      if (text(start + 1_default_int:start + 1_default_int) /= "[") then
         n = 2_default_int                 ! ESC followed by something else
         return
      end if

      j = start + 2_default_int
      do while (j <= last)
         byte = iachar(text(j:j), default_int)
         if (byte >= 64_default_int .and. byte <= 126_default_int) then
            n = j - start + 1_default_int  ! final byte, sequence complete
            return
         end if
         if (byte < 32_default_int .or. byte > 63_default_int) exit
         j = j + 1_default_int
      end do
      n = last - start + 1_default_int     ! unterminated; nowhere safe to cut
   end function escape_length

   pure function display_width(text) result(width)
      !! Number of columns `text` occupies.
      !!
      !! Counts UTF-8 code points and skips ANSI escape sequences, which take
      !! no space on screen. Continuation bytes, those with the top two bits
      !! `10`, do not start a character and so do not count. A byte that is
      !! not valid UTF-8 counts as one, so invalid input degrades to the byte
      !! count rather than to something arbitrary.
      !!
      !! One column per code point: an East Asian wide character occupies two
      !! and is counted once here. Getting that right needs a width table, and
      !! is out of scope.
      character(len=*), intent(in) :: text
         !! Text to measure.
      integer(default_int) :: width

      integer(default_int) :: i, byte, skip

      width = 0_default_int
      i = 1_default_int
      do while (i <= len(text, kind=default_int))
         skip = escape_length(text, i)
         if (skip > 0_default_int) then
            i = i + skip
            cycle
         end if
         byte = iachar(text(i:i), default_int)
         if (byte < 128_default_int .or. byte >= 192_default_int) then
            width = width + 1_default_int
         end if
         i = i + 1_default_int
      end do
   end function display_width

   pure function truncate_to_width(text, width) result(cut)
      !! Cut `text` to at most `width` columns, never mid-character and never
      !! inside an escape sequence.
      !!
      !! Escape sequences are kept and cost nothing, so a styled row keeps its
      !! colours and is cut by what it shows rather than by how it is spelled.
      !! Note that a cut can leave an attribute set with its reset dropped;
      !! `ansi_reset()` after writing a truncated row settles that.
      !!
      !! A `width` of zero still keeps nothing at all, escape sequences
      !! included: a caller asking for no columns wants no bytes.
      character(len=*), intent(in) :: text
         !! Text to truncate.
      integer(default_int), intent(in) :: width
         !! Maximum width in columns.
      character(len=:), allocatable :: cut

      integer(default_int) :: i, byte, seen, skip, last

      if (width <= 0_default_int) then
         cut = ""
         return
      end if

      seen = 0_default_int
      last = len(text, kind=default_int)
      i = 1_default_int
      do while (i <= last)
         skip = escape_length(text, i)
         if (skip > 0_default_int) then
            i = i + skip
            cycle
         end if
         byte = iachar(text(i:i), default_int)
         if (byte < 128_default_int .or. byte >= 192_default_int) then
            if (seen == width) then
               cut = text(1:i - 1_default_int)
               return
            end if
            seen = seen + 1_default_int
         end if
         i = i + 1_default_int
      end do
      cut = text
   end function truncate_to_width

   ! ---- frame composition ---------------------------------------------------

   subroutine frame_resize(this, rows, cols)
      !! Set the frame's dimensions, discarding the diff.
      !!
      !! A resize invalidates the previous frame: the terminal has thrown away
      !! what was on screen, so nothing can be assumed still to be there.
      class(frame_t), intent(inout) :: this
      integer(default_int), intent(in) :: rows
         !! Number of lines; below 0 is treated as 0.
      integer(default_int), intent(in) :: cols
         !! Width in code points; below 0 is treated as 0.

      integer(default_int) :: r, c, i

      r = max(0_default_int, rows)
      c = max(0_default_int, cols)
      if (allocated(this%current)) deallocate (this%current)
      if (allocated(this%shown)) deallocate (this%shown)
      this%rows = r
      this%cols = c
      allocate (this%current(r))
      allocate (this%shown(r))
      do i = 1_default_int, r
         this%current(i) = ""
         this%shown(i) = ""
      end do
      this%full_redraw = .true.
   end subroutine frame_resize

   pure function frame_row_count(this) result(r)
      !! Number of lines the frame holds.
      class(frame_t), intent(in) :: this
      integer(default_int) :: r

      r = this%rows
   end function frame_row_count

   pure function frame_col_count(this) result(c)
      !! Width the frame truncates to.
      class(frame_t), intent(in) :: this
      integer(default_int) :: c

      c = this%cols
   end function frame_col_count

   subroutine frame_set_line(this, row, text)
      !! Set one line, truncated to the frame width in code points.
      !!
      !! A row outside the frame is ignored rather than an error: a caller
      !! drawing a list longer than the terminal should not have to check
      !! every write.
      class(frame_t), intent(inout) :: this
      integer(default_int), intent(in) :: row
         !! One-based row.
      character(len=*), intent(in) :: text
         !! Text for the row.

      if (row < 1_default_int .or. row > this%rows) return
      this%current(row) = truncate_to_width(text, this%cols)
   end subroutine frame_set_line

   subroutine frame_invalidate(this)
      !! Force the next `render` to emit every row.
      !!
      !! For when something outside the frame has written to the screen, so
      !! the record of what is displayed can no longer be trusted.
      class(frame_t), intent(inout) :: this

      this%full_redraw = .true.
   end subroutine frame_invalidate

   function frame_render(this) result(output)
      !! The escape sequence that brings the screen up to date.
      !!
      !! Only rows whose text changed since the last render are emitted, each
      !! as a move, a line clear and the text. One string comes back and the
      !! caller writes it once: a single write cannot tear, where a write per
      !! row can.
      class(frame_t), intent(inout) :: this
      character(len=:), allocatable :: output

      integer(default_int) :: i

      output = ""
      if (this%rows == 0_default_int) then
         this%full_redraw = .false.
         return
      end if

      do i = 1_default_int, this%rows
         if (.not. this%full_redraw) then
            if (this%current(i) == this%shown(i)) cycle
         end if
         output = output//ansi_move_to(i, 1_default_int)//ansi_clear_line()// &
                  char(this%current(i))
         this%shown(i) = this%current(i)
      end do
      this%full_redraw = .false.
   end function frame_render

end module pic_ansi
