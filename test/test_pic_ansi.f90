module test_pic_ansi
   !! Tests for pic_ansi.
   !!
   !! No terminal is involved anywhere here, which is the point of the module:
   !! escape builders are pure functions over integers, the key decoder is a
   !! pure subroutine over a byte string, and the frame composer returns a
   !! string. Every case a real terminal could produce -- including an arrow
   !! key split across two reads -- is a literal in this file.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32
   use pic_ansi
   implicit none
   private

   public :: collect_pic_ansi_tests

   character(len=*), parameter :: CSI = ESC//"["

contains

   subroutine collect_pic_ansi_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("cursor_and_screen", test_cursor_and_screen), &
                  new_unittest("named_colours", test_named_colours), &
                  new_unittest("extended_colours", test_extended_colours), &
                  new_unittest("colour_clamping", test_colour_clamping), &
                  new_unittest("styled_wrapper", test_styled_wrapper), &
                  new_unittest("decode_printable", test_decode_printable), &
                  new_unittest("decode_control", test_decode_control), &
                  new_unittest("decode_arrows", test_decode_arrows), &
                  new_unittest("decode_application_mode", test_decode_application_mode), &
                  new_unittest("decode_numeric_forms", test_decode_numeric_forms), &
                  new_unittest("decode_split_sequence", test_decode_split_sequence), &
                  new_unittest("decode_lone_escape", test_decode_lone_escape), &
                  new_unittest("decode_unknown_resyncs", test_decode_unknown_resyncs), &
                  new_unittest("decode_utf8_passthrough", test_decode_utf8_passthrough), &
                  new_unittest("decode_overflow_is_safe", test_decode_overflow_is_safe), &
                  new_unittest("width_counts_code_points", test_width_counts_code_points), &
                  new_unittest("truncate_never_splits", test_truncate_never_splits), &
                  new_unittest("frame_first_render", test_frame_first_render), &
                  new_unittest("frame_diffs", test_frame_diffs), &
                  new_unittest("frame_invalidate", test_frame_invalidate), &
                  new_unittest("frame_resize", test_frame_resize), &
                  new_unittest("frame_truncates", test_frame_truncates), &
                  new_unittest("frame_edges", test_frame_edges), &
                  new_unittest("width_skips_escapes", test_width_skips_escapes), &
                  new_unittest("truncate_keeps_escapes", test_truncate_keeps_escapes), &
                  new_unittest("frame_holds_styled_rows", test_frame_holds_styled_rows), &
                  new_unittest("frame_rows_survive_blanking", test_frame_rows_survive_blanking), &
                  new_unittest("escape_then_sequence", test_escape_then_sequence), &
                  new_unittest("frame_diff_sees_trailing_blanks", test_frame_trailing_blanks), &
                  new_unittest("aborted_sequence_ends_there", test_aborted_sequence) &
                  ]
   end subroutine collect_pic_ansi_tests

   ! ---- escape builders -----------------------------------------------------

   subroutine test_cursor_and_screen(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, ansi_clear_screen() == CSI//"2J"//CSI//"H", "clear screen")
      if (allocated(error)) return
      call check(error, ansi_clear_line() == CSI//"2K", "clear line")
      if (allocated(error)) return
      call check(error, ansi_move_to(3_default_int, 14_default_int) == CSI//"3;14H", &
                 "move to row and column")
      if (allocated(error)) return
      call check(error, ansi_move_to(1_default_int, 1_default_int) == CSI//"1;1H", "move home")
      if (allocated(error)) return
      ! a zero or negative coordinate would produce a sequence the terminal
      ! misreads, so it is clamped rather than passed through
      call check(error, ansi_move_to(0_default_int, -4_default_int) == CSI//"1;1H", &
                 "coordinates below one are clamped")
      if (allocated(error)) return
      call check(error, ansi_move_to(120_default_int, 200_default_int) == CSI//"120;200H", &
                 "multi-digit coordinates")
      if (allocated(error)) return
      call check(error, ansi_hide_cursor() == CSI//"?25l", "hide cursor")
      if (allocated(error)) return
      call check(error, ansi_show_cursor() == CSI//"?25h", "show cursor")
      if (allocated(error)) return
      call check(error, ansi_alt_screen_enter() == CSI//"?1049h", "enter alt screen")
      if (allocated(error)) return
      call check(error, ansi_alt_screen_leave() == CSI//"?1049l", "leave alt screen")
      if (allocated(error)) return
      call check(error, ansi_bold() == CSI//"1m", "bold")
      if (allocated(error)) return
      call check(error, ansi_reverse() == CSI//"7m", "reverse")
      if (allocated(error)) return
      call check(error, ansi_reset() == CSI//"0m", "reset")
   end subroutine test_cursor_and_screen

   subroutine test_named_colours(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, ansi_fg(ANSI_BLACK) == CSI//"30m", "black foreground")
      if (allocated(error)) return
      call check(error, ansi_fg(ANSI_RED) == CSI//"31m", "red foreground")
      if (allocated(error)) return
      call check(error, ansi_fg(ANSI_WHITE) == CSI//"37m", "white foreground")
      if (allocated(error)) return
      ! the bright set is 90-97, not 38-45: an implementation that just added
      ! the index would run off the end of the normal range
      call check(error, ansi_fg(ANSI_BRIGHT_BLACK) == CSI//"90m", "bright black foreground")
      if (allocated(error)) return
      call check(error, ansi_fg(ANSI_BRIGHT_WHITE) == CSI//"97m", "bright white foreground")
      if (allocated(error)) return
      call check(error, ansi_bg(ANSI_BLACK) == CSI//"40m", "black background")
      if (allocated(error)) return
      call check(error, ansi_bg(ANSI_CYAN) == CSI//"46m", "cyan background")
      if (allocated(error)) return
      call check(error, ansi_bg(ANSI_BRIGHT_WHITE) == CSI//"107m", "bright white background")
   end subroutine test_named_colours

   subroutine test_extended_colours(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, ansi_fg_256(196_int32) == CSI//"38;5;196m", "256-colour foreground")
      if (allocated(error)) return
      call check(error, ansi_bg_256(0_int32) == CSI//"48;5;0m", "256-colour background")
      if (allocated(error)) return
      call check(error, ansi_fg_rgb(255_int32, 128_int32, 0_int32) == CSI//"38;2;255;128;0m", &
                 "24-bit foreground")
      if (allocated(error)) return
      call check(error, ansi_bg_rgb(1_int32, 2_int32, 3_int32) == CSI//"48;2;1;2;3m", &
                 "24-bit background")
   end subroutine test_extended_colours

   subroutine test_colour_clamping(error)
      !! An out-of-range value must not escape into the sequence, where the
      !! terminal would misparse it and the rest of the line with it.
      type(error_type), allocatable, intent(out) :: error

      call check(error, ansi_fg(-1_int32) == CSI//"30m", "a negative colour clamps to black")
      if (allocated(error)) return
      call check(error, ansi_fg(99_int32) == CSI//"97m", "a colour above 15 clamps to the top")
      if (allocated(error)) return
      call check(error, ansi_fg_256(-5_int32) == CSI//"38;5;0m", "a negative index clamps to 0")
      if (allocated(error)) return
      call check(error, ansi_fg_256(999_int32) == CSI//"38;5;255m", "an index above 255 clamps")
      if (allocated(error)) return
      call check(error, ansi_fg_rgb(-1_int32, 300_int32, 40_int32) == CSI//"38;2;0;255;40m", &
                 "rgb components clamp independently")
   end subroutine test_colour_clamping

   subroutine test_styled_wrapper(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, styled("hi", fg=ANSI_RED) == CSI//"31m"//"hi"//CSI//"0m", &
                 "foreground only")
      if (allocated(error)) return
      call check(error, styled("hi", fg=ANSI_RED, bold=.true.) == &
                 CSI//"31m"//CSI//"1m"//"hi"//CSI//"0m", "foreground and bold")
      if (allocated(error)) return
      call check(error, styled("hi", fg=ANSI_RED, bg=ANSI_BLUE) == &
                 CSI//"31m"//CSI//"44m"//"hi"//CSI//"0m", "foreground and background")
      if (allocated(error)) return
      ! nothing set means nothing emitted, not a stray reset
      call check(error, styled("hi") == "hi", "no attributes means no sequences at all")
      if (allocated(error)) return
      call check(error, styled("hi", bold=.false.) == "hi", "bold false emits nothing")
      if (allocated(error)) return
      call check(error, styled("") == "", "an empty string stays empty")
   end subroutine test_styled_wrapper

   ! ---- key decoding --------------------------------------------------------

   subroutine test_decode_printable(error)
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, "abc", events, n)
      call check(error, n == 3_default_int, "three bytes give three events")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_CHAR .and. events(1)%char_code == iachar("a"), &
                 "first byte")
      if (allocated(error)) return
      call check(error, events(3)%code == KEY_CHAR .and. events(3)%char_code == iachar("c"), &
                 "third byte")
      if (allocated(error)) return
      call check(error, pending%is_empty(), "nothing is left pending")
      if (allocated(error)) return

      call decode_keys(pending, "", events, n)
      call check(error, n == 0_default_int, "an empty read gives no events")
   end subroutine test_decode_printable

   subroutine test_decode_control(error)
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, achar(13), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_ENTER, "CR is Enter")
      if (allocated(error)) return
      call decode_keys(pending, achar(10), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_ENTER, "LF is Enter too")
      if (allocated(error)) return
      call decode_keys(pending, achar(127), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_BACKSPACE, "DEL is Backspace")
      if (allocated(error)) return
      call decode_keys(pending, achar(8), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_BACKSPACE, "BS is Backspace too")
      if (allocated(error)) return
      call decode_keys(pending, achar(9), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_TAB, "HT is Tab")
      if (allocated(error)) return
      call decode_keys(pending, achar(3), events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_CTRL_C, "ETX is Ctrl-C")
   end subroutine test_decode_control

   subroutine test_decode_arrows(error)
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, CSI//"A", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_UP, "up arrow")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"B", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_DOWN, "down arrow")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"C", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_RIGHT, "right arrow")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"D", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_LEFT, "left arrow")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"H", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_HOME, "Home")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"F", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_END, "End")
      if (allocated(error)) return

      ! several in one read, which is what a held-down key produces
      call decode_keys(pending, CSI//"A"//CSI//"A"//CSI//"B", events, n)
      call check(error, n == 3_default_int, "three arrows in one read")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_UP .and. events(2)%code == KEY_UP &
                 .and. events(3)%code == KEY_DOWN, "in order")
      if (allocated(error)) return

      ! mixed with text
      call decode_keys(pending, "a"//CSI//"A"//"b", events, n)
      call check(error, n == 3_default_int, "text around an arrow")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_CHAR .and. events(2)%code == KEY_UP &
                 .and. events(3)%code == KEY_CHAR, "text, arrow, text")
   end subroutine test_decode_arrows

   subroutine test_decode_application_mode(error)
      !! xterm sends ESC O A in application cursor mode and ESC [ A otherwise.
      !! A decoder that knew only one would lose the arrow keys in the other.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, ESC//"OA", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_UP, "ESC O A is up")
      if (allocated(error)) return
      call decode_keys(pending, ESC//"OD", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_LEFT, "ESC O D is left")
      if (allocated(error)) return
      call decode_keys(pending, ESC//"OH", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_HOME, "ESC O H is Home")
   end subroutine test_decode_application_mode

   subroutine test_decode_numeric_forms(error)
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, CSI//"3~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_DELETE, "ESC [ 3 ~ is Delete")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"1~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_HOME, "ESC [ 1 ~ is Home")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"7~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_HOME, "ESC [ 7 ~ is Home too")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"4~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_END, "ESC [ 4 ~ is End")
      if (allocated(error)) return
      call decode_keys(pending, CSI//"8~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_END, "ESC [ 8 ~ is End too")
      if (allocated(error)) return
      ! a numeric form followed by more input
      call decode_keys(pending, CSI//"3~"//"x", events, n)
      call check(error, n == 2_default_int, "Delete then a character")
      if (allocated(error)) return
      call check(error, events(2)%code == KEY_CHAR .and. events(2)%char_code == iachar("x"), &
                 "the tilde is consumed, not re-read")
   end subroutine test_decode_numeric_forms

   subroutine test_decode_split_sequence(error)
      !! The reason the decoder carries state: a terminal may hand over an
      !! arrow key one byte at a time.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, ESC, events, n)
      call check(error, n == 0_default_int, "a bare ESC yields nothing yet")
      if (allocated(error)) return
      call check(error,.not. pending%is_empty(), "and is held")
      if (allocated(error)) return

      call decode_keys(pending, "[", events, n)
      call check(error, n == 0_default_int, "ESC [ still yields nothing")
      if (allocated(error)) return

      call decode_keys(pending, "A", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_UP, &
                 "the third byte completes the arrow")
      if (allocated(error)) return
      call check(error, pending%is_empty(), "and the buffer is clear again")
      if (allocated(error)) return

      ! split two-and-one, the common case for a 1-byte read after a 2-byte one
      call decode_keys(pending, ESC//"[", events, n)
      call check(error, n == 0_default_int, "two bytes, no event")
      if (allocated(error)) return
      call decode_keys(pending, "D", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_LEFT, "one more completes it")
      if (allocated(error)) return

      ! a split numeric form, which needs four bytes
      call decode_keys(pending, CSI//"3", events, n)
      call check(error, n == 0_default_int, "ESC [ 3 is not yet Delete")
      if (allocated(error)) return
      call decode_keys(pending, "~", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_DELETE, "the tilde completes it")
   end subroutine test_decode_split_sequence

   subroutine test_decode_lone_escape(error)
      !! The user pressing Escape is indistinguishable from a truncated
      !! sequence until the next read says otherwise.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, ESC, events, n)
      call check(error, n == 0_default_int, "ESC alone reports nothing on the first read")
      if (allocated(error)) return

      ! an empty read is how a caller says "nothing followed it"
      call decode_keys(pending, "", events, n)
      call check(error, n == 1 .and. events(1)%code == KEY_ESC, &
                 "an empty read releases it as Escape")
      if (allocated(error)) return
      call check(error, pending%is_empty(), "and clears the buffer")
      if (allocated(error)) return

      ! a second empty read must not produce a phantom Escape
      call decode_keys(pending, "", events, n)
      call check(error, n == 0_default_int, "an empty read on an empty buffer is silent")
   end subroutine test_decode_lone_escape

   subroutine test_decode_unknown_resyncs(error)
      !! An unrecognised sequence must be dropped, not leave the decoder stuck.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      call decode_keys(pending, ESC//"[Z"//"x", events, n)
      call check(error, n == 1_default_int, "the unknown sequence produced no event")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_CHAR .and. events(1)%char_code == iachar("x"), &
                 "but the byte after it still decodes")
      if (allocated(error)) return
      call check(error, pending%is_empty(), "and nothing is left stuck in the buffer")
      if (allocated(error)) return

      ! ESC followed by an ordinary letter is not a sequence we know
      call decode_keys(pending, ESC//"q"//"y", events, n)
      ! Checked before indexing: if the decoder ever regressed to n == 0 this
      ! would read events(0) rather than fail cleanly.
      call check(error, n == 3_default_int, "ESC, then q, then y")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_ESC, "the ESC is the Escape key")
      if (allocated(error)) return
      call check(error, events(2)%code == KEY_CHAR .and. events(2)%char_code == iachar("q"), &
                 "the byte that ended it is not swallowed with it")
      if (allocated(error)) return
      call check(error, events(3)%code == KEY_CHAR .and. events(3)%char_code == iachar("y"), &
                 "decoding continues after an unknown ESC pair")
   end subroutine test_decode_unknown_resyncs

   subroutine test_decode_utf8_passthrough(error)
      !! Multi-byte UTF-8 comes through as its bytes, documented as version 1
      !! behaviour. What matters is that nothing is swallowed.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n
      character(len=*), parameter :: EURO = achar(226)//achar(130)//achar(172)

      call decode_keys(pending, EURO, events, n)
      call check(error, n == 3_default_int, "a three-byte character gives three events")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_CHAR .and. events(2)%code == KEY_CHAR &
                 .and. events(3)%code == KEY_CHAR, "all of them printable bytes")
      if (allocated(error)) return
      call check(error, events(1)%char_code == 226_int32, "the lead byte survives unmangled")
   end subroutine test_decode_utf8_passthrough

   subroutine test_decode_overflow_is_safe(error)
      !! A long unknown sequence must not overrun the pending buffer, and a
      !! full event array must not be written past.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(4)
      integer(default_int) :: n

      ! `ESC [ 1 2` completes at four bytes as an unknown sequence and is
      ! dropped; the rest is ordinary text and fills the four event slots.
      ! The old assertion here was `n >= 0`, which an event count always is.
      call decode_keys(pending, ESC//"[123456789012345678901234567890", events, n)
      call check(error, n == 4_default_int, "the remaining text fills the event array")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_CHAR .and. events(1)%char_code == iachar("3"), &
                 "and decoding resumes at the byte after the unknown sequence")
      if (allocated(error)) return

      call pending%clear()
      call decode_keys(pending, "abcdefghij", events, n)
      call check(error, n == 4_default_int, "more bytes than events fills only what fits")
      if (allocated(error)) return
      call check(error, events(4)%code == KEY_CHAR, "and the last slot is valid")
   end subroutine test_decode_overflow_is_safe

   ! ---- width ---------------------------------------------------------------

   subroutine test_width_counts_code_points(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: BOX = achar(226)//achar(148)//achar(128)
      character(len=*), parameter :: EACUTE = achar(195)//achar(169)

      call check(error, display_width("") == 0_default_int, "empty is zero wide")
      if (allocated(error)) return
      call check(error, display_width("abc") == 3_default_int, "ascii is one per byte")
      if (allocated(error)) return
      call check(error, len(BOX) == 3, "the box character really is three bytes")
      if (allocated(error)) return
      call check(error, display_width(BOX) == 1_default_int, "but one code point wide")
      if (allocated(error)) return
      call check(error, display_width(BOX//BOX//BOX) == 3_default_int, "three of them, three wide")
      if (allocated(error)) return
      call check(error, display_width("a"//EACUTE//"b") == 3_default_int, "mixed ascii and two-byte")
   end subroutine test_width_counts_code_points

   subroutine test_truncate_never_splits(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: BOX = achar(226)//achar(148)//achar(128)
      character(len=:), allocatable :: cut

      cut = truncate_to_width("abcdef", 3_default_int)
      call check(error, cut == "abc", "ascii truncates by count")
      if (allocated(error)) return

      cut = truncate_to_width("abc", 10_default_int)
      call check(error, cut == "abc", "a short string is returned whole")
      if (allocated(error)) return

      cut = truncate_to_width("abc", 0_default_int)
      call check(error, cut == "", "a width of zero gives nothing")
      if (allocated(error)) return

      cut = truncate_to_width("abc", -1_default_int)
      call check(error, cut == "", "a negative width gives nothing")
      if (allocated(error)) return

      ! the case the whole thing exists for: cutting three box characters to
      ! two must drop three whole bytes, not cut the second one in half
      cut = truncate_to_width(BOX//BOX//BOX, 2_default_int)
      call check(error, display_width(cut) == 2_default_int, "two code points survive")
      if (allocated(error)) return
      call check(error, len(cut) == 6, "and they are six whole bytes")
      if (allocated(error)) return
      call check(error, cut == BOX//BOX, "byte for byte the first two characters")
   end subroutine test_truncate_never_splits

   ! ---- frame ---------------------------------------------------------------

   subroutine test_frame_first_render(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(3_default_int, 20_default_int)
      call check(error, frame%row_count() == 3_default_int, "rows are remembered")
      if (allocated(error)) return
      call check(error, frame%col_count() == 20_default_int, "columns are remembered")
      if (allocated(error)) return

      call frame%set_line(1_default_int, "one")
      call frame%set_line(2_default_int, "two")
      call frame%set_line(3_default_int, "three")
      out = frame%render()

      call check(error, index(out, "one") > 0 .and. index(out, "two") > 0 &
                 .and. index(out, "three") > 0, "the first render emits every row")
      if (allocated(error)) return
      call check(error, index(out, CSI//"1;1H") > 0, "with a move for row 1")
      if (allocated(error)) return
      call check(error, index(out, CSI//"3;1H") > 0, "and a move for row 3")
      if (allocated(error)) return
      call check(error, index(out, CSI//"2K") > 0, "and a line clear")
   end subroutine test_frame_first_render

   subroutine test_frame_diffs(error)
      !! The point of the frame: an idle board that updates a clock writes one
      !! line, not the whole screen.
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(4_default_int, 20_default_int)
      call frame%set_line(1_default_int, "header")
      call frame%set_line(2_default_int, "body")
      call frame%set_line(3_default_int, "12:00:00")
      call frame%set_line(4_default_int, "footer")
      out = frame%render()
      call check(error, len(out) > 0, "the first render is not empty")
      if (allocated(error)) return

      ! nothing changed
      out = frame%render()
      call check(error, len(out) == 0, "an unchanged frame renders to nothing at all")
      if (allocated(error)) return

      ! one line changed
      call frame%set_line(3_default_int, "12:00:01")
      out = frame%render()
      call check(error, index(out, "12:00:01") > 0, "the changed line is emitted")
      if (allocated(error)) return
      call check(error, index(out, "header") == 0, "an unchanged line above is not")
      if (allocated(error)) return
      call check(error, index(out, "footer") == 0, "nor one below")
      if (allocated(error)) return
      call check(error, index(out, CSI//"3;1H") > 0, "and the cursor moves to that row")
      if (allocated(error)) return

      ! and it settles again
      out = frame%render()
      call check(error, len(out) == 0, "rendering again after the diff is empty")
   end subroutine test_frame_diffs

   subroutine test_frame_invalidate(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(2_default_int, 20_default_int)
      call frame%set_line(1_default_int, "alpha")
      call frame%set_line(2_default_int, "beta")
      out = frame%render()
      out = frame%render()
      call check(error, len(out) == 0, "settled")
      if (allocated(error)) return

      call frame%invalidate()
      out = frame%render()
      call check(error, index(out, "alpha") > 0 .and. index(out, "beta") > 0, &
                 "invalidate forces every row out again")
   end subroutine test_frame_invalidate

   subroutine test_frame_resize(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(2_default_int, 20_default_int)
      call frame%set_line(1_default_int, "alpha")
      out = frame%render()

      ! a resize throws away what the terminal was showing, so the next render
      ! cannot assume anything is still there
      call frame%resize(3_default_int, 10_default_int)
      call check(error, frame%row_count() == 3_default_int, "the new row count takes")
      if (allocated(error)) return
      call check(error, frame%col_count() == 10_default_int, "and the new width")
      if (allocated(error)) return

      call frame%set_line(1_default_int, "alpha")
      out = frame%render()
      call check(error, index(out, "alpha") > 0, &
                 "the same text is re-emitted after a resize, not diffed away")
   end subroutine test_frame_resize

   subroutine test_frame_truncates(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out
      character(len=*), parameter :: BOX = achar(226)//achar(148)//achar(128)

      call frame%resize(1_default_int, 5_default_int)
      call frame%set_line(1_default_int, "abcdefghij")
      out = frame%render()
      call check(error, index(out, "abcde") > 0, "the first five characters are kept")
      if (allocated(error)) return
      call check(error, index(out, "abcdef") == 0, "and the sixth is not")
      if (allocated(error)) return

      ! a row of box characters is 15 bytes but 5 columns, and must survive
      call frame%resize(1_default_int, 5_default_int)
      call frame%set_line(1_default_int, BOX//BOX//BOX//BOX//BOX//BOX//BOX)
      out = frame%render()
      call check(error, index(out, BOX//BOX//BOX//BOX//BOX) > 0, &
                 "five whole box characters survive a width of five")
   end subroutine test_frame_truncates

   subroutine test_frame_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      ! a zero-row frame must render to nothing rather than fail
      call frame%resize(0_default_int, 0_default_int)
      out = frame%render()
      call check(error, len(out) == 0, "an empty frame renders to nothing")
      if (allocated(error)) return

      ! writes outside the frame are ignored, so a caller drawing a list
      ! longer than the terminal need not check every row
      call frame%resize(2_default_int, 10_default_int)
      call frame%set_line(0_default_int, "above")
      call frame%set_line(5_default_int, "below")
      out = frame%render()
      call check(error, index(out, "above") == 0 .and. index(out, "below") == 0, &
                 "out-of-range rows are dropped, not written somewhere else")
      if (allocated(error)) return

      ! negative dimensions are clamped rather than rejected
      call frame%resize(-3_default_int, -7_default_int)
      call check(error, frame%row_count() == 0_default_int, "negative rows clamp to zero")
      if (allocated(error)) return
      call check(error, frame%col_count() == 0_default_int, "negative columns clamp to zero")
   end subroutine test_frame_edges

   subroutine test_width_skips_escapes(error)
      !! An escape sequence takes no columns. Counting its bytes made
      !! `styled("ARRIVALS", fg=ANSI_RED)` measure seventeen wide instead of
      !! eight, which is the width the module's own quick start relies on.
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: painted

      painted = styled("ARRIVALS", fg=ANSI_RED)

      call check(error, len(painted) == 17, "the styled string really is seventeen bytes")
      if (allocated(error)) return
      call check(error, display_width(painted) == 8_default_int, &
                 "but eight columns wide")
      if (allocated(error)) return

      call check(error, display_width(ansi_reset()) == 0_default_int, &
                 "a bare escape sequence is zero wide")
      if (allocated(error)) return
      call check(error, display_width(ansi_move_to(12_default_int, 34_default_int)) == 0_default_int, &
                 "including one with multi-digit parameters")
      if (allocated(error)) return
      call check(error, display_width(styled("x", fg=ANSI_RED, bg=ANSI_BLUE, bold=.true.)) &
                 == 1_default_int, "three attributes and a reset still leave one column")
      if (allocated(error)) return

      ! a lone ESC, and an unterminated sequence, must not run off the end
      call check(error, display_width(ESC) == 0_default_int, "a trailing ESC is consumed")
      if (allocated(error)) return
      call check(error, display_width(CSI//"12") == 0_default_int, &
                 "an unterminated sequence consumes the rest")
   end subroutine test_width_skips_escapes

   subroutine test_truncate_keeps_escapes(error)
      !! Cutting inside an escape sequence does not merely lose a colour: the
      !! terminal reads the rest of the row as parameters.
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: cut

      cut = truncate_to_width(styled("ARRIVALS", fg=ANSI_RED), 4_default_int)
      call check(error, display_width(cut) == 4_default_int, "four columns survive")
      if (allocated(error)) return
      call check(error, index(cut, CSI//"31m") > 0, "and the colour that preceded them")
      if (allocated(error)) return
      call check(error, index(cut, "ARRI") > 0, "with the right four characters")
      if (allocated(error)) return
      call check(error, index(cut, "ARRIV") == 0, "and not a fifth")
      if (allocated(error)) return

      ! the cut must never land inside a sequence
      cut = truncate_to_width(CSI//"31m"//"abc", 2_default_int)
      call check(error, cut == CSI//"31m"//"ab", "the sequence is kept whole")
      if (allocated(error)) return

      ! a styled string shorter than the width comes back untouched
      cut = truncate_to_width(styled("hi", fg=ANSI_GREEN), 10_default_int)
      call check(error, cut == styled("hi", fg=ANSI_GREEN), "no truncation, no change")
      if (allocated(error)) return

      cut = truncate_to_width(styled("hi", fg=ANSI_GREEN), 0_default_int)
      call check(error, cut == "", "a width of zero keeps nothing, escapes included")
   end subroutine test_truncate_keeps_escapes

   subroutine test_frame_holds_styled_rows(error)
      !! The frame's rows are `string_type`, so there is no byte-length cap to
      !! overrun. Per-character colouring runs to about ten bytes a column,
      !! which any fixed multiple of the width would have truncated.
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out, loud
      integer(default_int) :: i

      loud = ""
      do i = 1_default_int, 40_default_int
         loud = loud//styled("x", fg=ANSI_RED, bg=ANSI_BLUE, bold=.true.)
      end do
      call check(error, display_width(loud) == 40_default_int, "forty columns")
      if (allocated(error)) return
      call check(error, len(loud) > 400, "but well over four hundred bytes")
      if (allocated(error)) return

      call frame%resize(1_default_int, 40_default_int)
      call frame%set_line(1_default_int, loud)
      out = frame%render()
      call check(error, index(out, loud) > 0, &
                 "the whole styled row survives, uncapped and untruncated")
   end subroutine test_frame_holds_styled_rows

   subroutine test_frame_rows_survive_blanking(error)
      !! A regression guard. The rows used to be a deferred-length allocatable
      !! character array blanked with a whole-array `= ""`, which F2018
      !! 10.2.1.3 lets a compiler read as "reallocate to length zero" -- GNU,
      !! AOCC and LFortran kept the length, Intel and NVIDIA did not, and every
      !! row came out empty there.
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(3_default_int, 40_default_int)

      ! the first thing written after a resize must survive
      call frame%set_line(1_default_int, "first")
      out = frame%render()
      call check(error, index(out, "first") > 0, &
                 "a row written straight after resize is not blanked away")
      if (allocated(error)) return

      ! and a resize must genuinely clear what was there
      call frame%resize(3_default_int, 40_default_int)
      out = frame%render()
      call check(error, index(out, "first") == 0, "a resize really does clear the rows")
      if (allocated(error)) return

      ! a long row, well past any plausible fixed buffer
      call frame%resize(1_default_int, 200_default_int)
      call frame%set_line(1_default_int, repeat("y", 200))
      out = frame%render()
      call check(error, index(out, repeat("y", 200)) > 0, "two hundred columns survive intact")
   end subroutine test_frame_rows_survive_blanking

   subroutine test_escape_then_sequence(error)
      !! Escape followed by anything that cannot continue a sequence used to
      !! discard both bytes. `ESC ESC [ A` came out as the characters `[` and
      !! `A`; a held Escape followed by `q` lost the Escape and the `q`.
      type(error_type), allocatable, intent(out) :: error
      type(pending_t) :: pending
      type(key_event_t) :: events(16)
      integer(default_int) :: n

      ! Escape then Up, in one read
      call decode_keys(pending, ESC//ESC//"[A", events, n)
      call check(error, n == 2_default_int, "Escape then Up is two events")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_ESC, "the first is Escape")
      if (allocated(error)) return
      call check(error, events(2)%code == KEY_UP, "the second is the arrow, not two characters")
      if (allocated(error)) return

      ! Escape pressed twice
      call pending%clear()
      call decode_keys(pending, ESC//ESC, events, n)
      call check(error, n == 1_default_int, "the first of two Escapes is reported at once")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_ESC, "and it is an Escape")
      if (allocated(error)) return
      call decode_keys(pending, "", events, n)
      call check(error, n == 1_default_int .and. events(1)%code == KEY_ESC, &
                 "the second is released by the empty read")
      if (allocated(error)) return

      ! a held Escape, then ordinary text in the next read
      call pending%clear()
      call decode_keys(pending, ESC, events, n)
      call check(error, n == 0_default_int, "the Escape is held")
      if (allocated(error)) return
      call decode_keys(pending, "q", events, n)
      call check(error, n == 2_default_int, "the next read releases it and decodes the byte")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_ESC .and. events(2)%code == KEY_CHAR &
                 .and. events(2)%char_code == iachar("q"), "Escape then q, neither lost")
      if (allocated(error)) return

      ! a held Escape, then a real sequence in the next read
      call pending%clear()
      call decode_keys(pending, ESC, events, n)
      call decode_keys(pending, CSI//"D", events, n)
      call check(error, n == 2_default_int, "Escape then Left across two reads")
      if (allocated(error)) return
      call check(error, events(1)%code == KEY_ESC .and. events(2)%code == KEY_LEFT, &
                 "and the arrow still decodes")
   end subroutine test_escape_then_sequence

   subroutine test_frame_trailing_blanks(error)
      !! `string_type`'s `==` is blank-padded character comparison, so "abc"
      !! and "abc   " compare equal. They are the same text but not the same
      !! row: with a background colour open at the end of the line, those
      !! three cells are painted.
      type(error_type), allocatable, intent(out) :: error
      type(frame_t) :: frame
      character(len=:), allocatable :: out

      call frame%resize(1_default_int, 20_default_int)
      call frame%set_line(1_default_int, ansi_bg(ANSI_BLUE)//"abc")
      out = frame%render()
      call check(error, len(out) > 0, "the first render emits the row")
      if (allocated(error)) return

      out = frame%render()
      call check(error, len(out) == 0, "an unchanged row still renders to nothing")
      if (allocated(error)) return

      call frame%set_line(1_default_int, ansi_bg(ANSI_BLUE)//"abc   ")
      out = frame%render()
      call check(error, len(out) > 0, &
                 "three more painted cells is a change, not a no-op")
   end subroutine test_frame_trailing_blanks

   subroutine test_aborted_sequence(error)
      !! A CSI aborted by a byte that cannot appear in one ends there, as it
      !! does in a terminal. Treating it as unterminated swallowed the rest of
      !! the row.
      type(error_type), allocatable, intent(out) :: error

      call check(error, display_width(CSI//"3"//ESC//"[31m"//"abc") == 3_default_int, &
                 "text after an aborted sequence is still three columns")
      if (allocated(error)) return
      call check(error, display_width(CSI//"12") == 0_default_int, &
                 "but a sequence that runs off the end still swallows the rest")
      if (allocated(error)) return
      call check(error, truncate_to_width(CSI//"3"//ESC//"[31m"//"abc", 1_default_int) &
                 /= CSI//"3"//ESC//"[31m"//"abc", "and truncation is not fooled by it")
   end subroutine test_aborted_sequence

end module test_pic_ansi
