! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! String splitting and checked numeric parsing.
module pic_tokenizer
   !! Splitting of character data into tokens and parsing of those tokens into
   !! numbers with explicit error reporting through `error_t`.
   !!
   !! The parsing routines never fail silently: every malformed input produces
   !! an `ERROR_PARSE` error with a human readable message, and the output
   !! value is set to zero. They deliberately do **not** use list directed
   !! internal reads, because those accept input that is almost never what the
   !! caller meant (`"1,2"` reads as 1, `"3*7"` is a repeat count, `"nan"` and
   !! `"infinity"` are accepted, and trailing junk after a separator is
   !! ignored). Every accepted form is checked against an explicit grammar
   !! first.
   !!
   !! ### Empty field policy of `split`
   !!
   !! `split` keeps empty fields, so the number of returned elements is always
   !! the number of delimiter occurrences plus one:
   !!
   !! * `split("a,,b", ",")` returns three elements, `"a"`, `""` and `"b"`
   !! * `split("", ",")` returns one element, the empty string
   !! * `split(",a", ",")` returns `""` and `"a"`, `split("a,", ",")` returns
   !!   `"a"` and `""`
   !! * `split("abc", ",")` returns one element, `"abc"`
   !!
   !! This makes `join(split(text, delim), delim)` reproduce `text` exactly
   !! (for fields without trailing blanks) and keeps positional data such as
   !! CSV records aligned. Use `tokenize` for the other common policy, where
   !! runs of separators collapse and empty fields are dropped.
   !!
   !! A zero length delimiter cannot match anything, so `split(text, "")`
   !! returns the whole of `text` as a single element.
   use pic_ascii, only: whitespace, digits
   use pic_error, only: error_t, ERROR_PARSE
   use pic_string_type, only: string_type, char
   use pic_strings, only: strip
   use pic_types, only: default_int, int32, int64, sp, dp
   implicit none
   private

   public :: split
   public :: tokenize
   public :: parse_int
   public :: parse_real

   character(len=*), parameter :: SIGNS = "+-"
      !! Characters accepted as a leading sign of a number or of an exponent
   character(len=*), parameter :: EXPONENT_MARKERS = "eEdD"
      !! Exponent letters accepted by `parse_real`; `q`/`Q` is not accepted

   !> Split a string on every occurrence of a delimiter, keeping empty fields.
   !>
   !> The result always holds one more element than there are delimiter
   !> occurrences, so an empty input yields one empty element. See the module
   !> documentation for the full policy.
   interface split
      module procedure :: split_char_char
      module procedure :: split_char_string
      module procedure :: split_string_char
      module procedure :: split_string_string
   end interface split

   !> Split a string into whitespace separated words.
   !>
   !> Runs of whitespace are treated as a single separator and empty fields are
   !> discarded, so a string that is empty or entirely whitespace yields a zero
   !> sized result. Whitespace is the set defined by `pic_ascii`, that is space,
   !> horizontal tab, vertical tab, carriage return, line feed and form feed.
   interface tokenize
      module procedure :: tokenize_char
      module procedure :: tokenize_string
   end interface tokenize

   !> Parse a string into an integer, reporting failures through `error_t`.
   !>
   !> The accepted grammar is optional surrounding whitespace, an optional `+`
   !> or `-` sign and one or more decimal digits, with nothing else. An empty
   !> or blank string, a missing digit, any stray character (including trailing
   !> junk such as `"12abc"`) and a value too large for the requested kind all
   !> set `ERROR_PARSE` and leave `value` at zero.
   !>
   !> Specific procedures exist for `int32` and `int64`, so an argument
   !> declared `integer(default_int)` resolves correctly in both the default
   !> and the `USE_INT8` build.
   interface parse_int
      module procedure :: parse_int_int32_char
      module procedure :: parse_int_int32_string
      module procedure :: parse_int_int64_char
      module procedure :: parse_int_int64_string
   end interface parse_int

   !> Parse a string into a real, reporting failures through `error_t`.
   !>
   !> The accepted grammar is optional surrounding whitespace, an optional
   !> sign, a mantissa holding at least one digit and at most one decimal point
   !> (so both `".5"` and `"5."` are accepted, `"."` is not), and an optional
   !> exponent introduced by `e`, `E`, `d` or `D` with an optional sign and at
   !> least one digit.
   !>
   !> Everything else is rejected, in particular the spellings that a list
   !> directed read would happily accept: `"nan"`, `"inf"`, `"infinity"`,
   !> repeat counts such as `"3*7"`, comma separated values such as `"1,2"`,
   !> and any trailing junk. Values that overflow the requested kind are
   !> rejected as well instead of becoming infinity.
   !>
   !> Specific procedures exist for `sp` and `dp`.
   interface parse_real
      module procedure :: parse_real_sp_char
      module procedure :: parse_real_sp_string
      module procedure :: parse_real_dp_char
      module procedure :: parse_real_dp_string
   end interface parse_real

contains

   pure function split_char_char(string, delimiter) result(parts)
      !! Split a character string on a character delimiter.
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      type(string_type), allocatable :: parts(:)

      integer(default_int) :: n_parts, len_delim, pos, hit, i_part

      len_delim = len(delimiter)
      if (len_delim == 0) then
         allocate (parts(1))
         parts(1) = string_type(string)
         return
      end if

      n_parts = 1
      pos = 1
      do
         hit = index(string(pos:), delimiter)
         if (hit == 0) exit
         n_parts = n_parts + 1
         pos = pos + hit - 1 + len_delim
      end do

      allocate (parts(n_parts))
      pos = 1
      do i_part = 1, n_parts - 1
         hit = index(string(pos:), delimiter)
         parts(i_part) = string_type(string(pos:pos + hit - 2))
         pos = pos + hit - 1 + len_delim
      end do
      parts(n_parts) = string_type(string(pos:))

   end function split_char_char

   pure function split_char_string(string, delimiter) result(parts)
      !! Split a character string on a `string_type` delimiter.
      character(len=*), intent(in) :: string
      type(string_type), intent(in) :: delimiter
      type(string_type), allocatable :: parts(:)

      parts = split_char_char(string, char(delimiter))

   end function split_char_string

   pure function split_string_char(string, delimiter) result(parts)
      !! Split a `string_type` on a character delimiter.
      type(string_type), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      type(string_type), allocatable :: parts(:)

      parts = split_char_char(char(string), delimiter)

   end function split_string_char

   pure function split_string_string(string, delimiter) result(parts)
      !! Split a `string_type` on a `string_type` delimiter.
      type(string_type), intent(in) :: string
      type(string_type), intent(in) :: delimiter
      type(string_type), allocatable :: parts(:)

      parts = split_char_char(char(string), char(delimiter))

   end function split_string_string

   pure function tokenize_char(string) result(tokens)
      !! Split a character string into whitespace separated words.
      character(len=*), intent(in) :: string
      type(string_type), allocatable :: tokens(:)

      integer(default_int), allocatable :: first(:), last(:)
      integer(default_int) :: n_chars, n_tokens, pos, offset, i_token

      n_chars = len(string)
      ! A token needs at least one character and one separator, so there can
      ! never be more than (n + 2)/2 of them; that bound also covers n == 0.
      allocate (first((n_chars + 2)/2), last((n_chars + 2)/2))

      n_tokens = 0
      pos = 1
      do
         offset = verify(string(pos:), whitespace)
         if (offset == 0) exit
         n_tokens = n_tokens + 1
         first(n_tokens) = pos + offset - 1
         offset = scan(string(first(n_tokens):), whitespace)
         if (offset == 0) then
            last(n_tokens) = n_chars
         else
            last(n_tokens) = first(n_tokens) + offset - 2
         end if
         pos = last(n_tokens) + 1
      end do

      allocate (tokens(n_tokens))
      do i_token = 1, n_tokens
         tokens(i_token) = string_type(string(first(i_token):last(i_token)))
      end do

   end function tokenize_char

   pure function tokenize_string(string) result(tokens)
      !! Split a `string_type` into whitespace separated words.
      type(string_type), intent(in) :: string
      type(string_type), allocatable :: tokens(:)

      tokens = tokenize_char(char(string))

   end function tokenize_string

   pure subroutine parse_int_int64_char(str, value, err)
      !! Parse a character string into a 64 bit integer.
      character(len=*), intent(in) :: str
      integer(int64), intent(out) :: value
         !! Fixed width on purpose: this is the widest integer the parser
         !! accumulates in, and the narrower specific procedure range checks
         !! against it.
      type(error_t), intent(out) :: err

      call parse_integer_wide(str, value, err)

   end subroutine parse_int_int64_char

   pure subroutine parse_int_int64_string(str, value, err)
      !! Parse a `string_type` into a 64 bit integer.
      type(string_type), intent(in) :: str
      integer(int64), intent(out) :: value
      type(error_t), intent(out) :: err

      call parse_integer_wide(char(str), value, err)

   end subroutine parse_int_int64_string

   pure subroutine parse_int_int32_char(str, value, err)
      !! Parse a character string into a 32 bit integer.
      character(len=*), intent(in) :: str
      integer(int32), intent(out) :: value
         !! Fixed width on purpose: the narrow specific procedure of the
         !! generic.
      type(error_t), intent(out) :: err

      integer(int64) :: wide

      value = 0_int32
      call parse_integer_wide(str, wide, err)
      if (err%has_error()) return

      if (wide > int(huge(value), int64) .or. wide < -int(huge(value), int64) - 1_int64) then
         call err%set(ERROR_PARSE, "integer out of range for a 32 bit integer: '"//strip(str)//"'")
         return
      end if
      value = int(wide, int32)

   end subroutine parse_int_int32_char

   pure subroutine parse_int_int32_string(str, value, err)
      !! Parse a `string_type` into a 32 bit integer.
      type(string_type), intent(in) :: str
      integer(int32), intent(out) :: value
      type(error_t), intent(out) :: err

      call parse_int_int32_char(char(str), value, err)

   end subroutine parse_int_int32_string

   pure subroutine parse_integer_wide(str, value, err)
      !! Core integer parser, accumulating in `int64`.
      !!
      !! The accumulation runs in the negative half of the integer range so
      !! that the most negative representable value can be parsed without ever
      !! evaluating an expression that overflows.
      character(len=*), intent(in) :: str
      integer(int64), intent(out) :: value
      type(error_t), intent(out) :: err

      character(len=:), allocatable :: text
      integer(default_int) :: pos, i_char
      integer(int64) :: accumulator, limit, mult_limit, digit
      logical :: negative

      value = 0_int64
      text = strip(str)
      if (len(text) == 0) then
         call err%set(ERROR_PARSE, "cannot parse an empty string as an integer")
         return
      end if

      pos = 1
      negative = (text(1:1) == "-")
      if (index(SIGNS, text(1:1)) > 0) pos = 2

      if (pos > len(text)) then
         call err%set(ERROR_PARSE, "no digits after the sign in '"//text//"'")
         return
      end if

      if (negative) then
         limit = -huge(accumulator) - 1_int64
      else
         limit = -huge(accumulator)
      end if
      mult_limit = limit/10_int64
      accumulator = 0_int64

      do i_char = pos, len(text)
         digit = int(index(digits, text(i_char:i_char)), int64) - 1_int64
         if (digit < 0_int64) then
            call err%set(ERROR_PARSE, "invalid character '"//text(i_char:i_char)// &
                         "' while parsing the integer '"//text//"'")
            return
         end if
         if (accumulator < mult_limit) then
            call err%set(ERROR_PARSE, "integer overflow while parsing '"//text//"'")
            return
         end if
         accumulator = accumulator*10_int64
         if (accumulator < limit + digit) then
            call err%set(ERROR_PARSE, "integer overflow while parsing '"//text//"'")
            return
         end if
         accumulator = accumulator - digit
      end do

      if (negative) then
         value = accumulator
      else
         value = -accumulator
      end if

   end subroutine parse_integer_wide

   subroutine parse_real_dp_char(str, value, err)
      !! Parse a character string into a double precision real.
      character(len=*), intent(in) :: str
      real(dp), intent(out) :: value
      type(error_t), intent(out) :: err

      call parse_real_wide(str, value, err)

   end subroutine parse_real_dp_char

   subroutine parse_real_dp_string(str, value, err)
      !! Parse a `string_type` into a double precision real.
      type(string_type), intent(in) :: str
      real(dp), intent(out) :: value
      type(error_t), intent(out) :: err

      call parse_real_wide(char(str), value, err)

   end subroutine parse_real_dp_string

   subroutine parse_real_sp_char(str, value, err)
      !! Parse a character string into a single precision real.
      !!
      !! The conversion is done in double precision and then range checked, so
      !! a value that is finite in `dp` but too large for `sp` is reported as
      !! an error instead of becoming infinity.
      character(len=*), intent(in) :: str
      real(sp), intent(out) :: value
      type(error_t), intent(out) :: err

      real(dp) :: wide

      value = 0.0_sp
      call parse_real_wide(str, wide, err)
      if (err%has_error()) return

      if (abs(wide) > real(huge(value), dp)) then
         call err%set(ERROR_PARSE, "real out of range for single precision: '"//strip(str)//"'")
         return
      end if
      value = real(wide, sp)

   end subroutine parse_real_sp_char

   subroutine parse_real_sp_string(str, value, err)
      !! Parse a `string_type` into a single precision real.
      type(string_type), intent(in) :: str
      real(sp), intent(out) :: value
      type(error_t), intent(out) :: err

      call parse_real_sp_char(char(str), value, err)

   end subroutine parse_real_sp_string

   subroutine parse_real_wide(str, value, err)
      !! Core real parser.
      !!
      !! The string is validated against an explicit grammar first and only
      !! then converted with an `F` edit descriptor built for the exact field
      !! width. List directed input is avoided on purpose, see the module
      !! documentation.
      character(len=*), intent(in) :: str
      real(dp), intent(out) :: value
      type(error_t), intent(out) :: err

      character(len=:), allocatable :: text
      character(len=16) :: field_format
      integer(default_int) :: pos, n_digits
      integer(int32) :: stat
         !! Fixed width on purpose: `iostat` takes a default integer, which is
         !! `int32` regardless of how PIC defines `default_int`.
      logical :: seen_point

      value = 0.0_dp
      text = strip(str)
      if (len(text) == 0) then
         call err%set(ERROR_PARSE, "cannot parse an empty string as a real")
         return
      end if

      pos = 1
      call skip_sign(text, pos)

      n_digits = 0
      seen_point = .false.
      do while (pos <= len(text))
         if (index(digits, text(pos:pos)) > 0) then
            n_digits = n_digits + 1
         else if (text(pos:pos) == "." .and. .not. seen_point) then
            seen_point = .true.
         else
            exit
         end if
         pos = pos + 1
      end do

      if (n_digits == 0) then
         call err%set(ERROR_PARSE, "no digits in the mantissa of '"//text//"'")
         return
      end if

      if (pos <= len(text)) then
         call check_exponent(text, pos, err)
         if (err%has_error()) return
      end if

      write (field_format, '("(F",I0,".0)")') len(text)
      read (text, field_format, iostat=stat) value
      ! A failed read leaves `value` undefined, so make it safe to inspect
      ! before the range check below looks at it.
      if (stat /= 0_int32) value = 0.0_dp

      ! Compilers disagree on what an out of range field does: some report a
      ! non zero iostat, others quietly deliver an infinity. Reject both.
      if (stat /= 0_int32 .or. abs(value) > huge(value)) then
         value = 0.0_dp
         call err%set(ERROR_PARSE, "real out of range or not convertible: '"//text//"'")
         return
      end if

   end subroutine parse_real_wide

   pure subroutine check_exponent(text, pos, err)
      !! Validate the exponent part of a real, starting at `pos`.
      character(len=*), intent(in) :: text
      integer(default_int), intent(in) :: pos
      type(error_t), intent(inout) :: err

      integer(default_int) :: scan_pos

      if (index(EXPONENT_MARKERS, text(pos:pos)) == 0) then
         call err%set(ERROR_PARSE, "invalid character '"//text(pos:pos)// &
                      "' while parsing the real '"//text//"'")
         return
      end if

      scan_pos = pos + 1
      call skip_sign(text, scan_pos)
      if (scan_pos > len(text)) then
         call err%set(ERROR_PARSE, "missing exponent digits in '"//text//"'")
         return
      end if

      do while (scan_pos <= len(text))
         if (index(digits, text(scan_pos:scan_pos)) == 0) then
            call err%set(ERROR_PARSE, "invalid character '"//text(scan_pos:scan_pos)// &
                         "' in the exponent of '"//text//"'")
            return
         end if
         scan_pos = scan_pos + 1
      end do

   end subroutine check_exponent

   pure subroutine skip_sign(text, pos)
      !! Advance `pos` past a leading `+` or `-` if one is present.
      character(len=*), intent(in) :: text
      integer(default_int), intent(inout) :: pos

      if (pos > len(text)) return
      if (index(SIGNS, text(pos:pos)) > 0) pos = pos + 1

   end subroutine skip_sign

end module pic_tokenizer
