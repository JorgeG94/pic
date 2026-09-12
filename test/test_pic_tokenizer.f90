module test_pic_tokenizer
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_tokenizer, only: split, tokenize, parse_int, parse_real
   use pic_error, only: error_t, ERROR_PARSE
   use pic_string_type, only: string_type, char
   use pic_strings, only: join
   use pic_ascii, only: TAB, LF, CR, VT, FF
   use pic_types, only: int32, int64, sp, dp
   implicit none
   private
   public :: collect_pic_tokenizer_tests

contains

   subroutine collect_pic_tokenizer_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("split_basic", test_split_basic), &
                  new_unittest("split_empty_fields", test_split_empty_fields), &
                  new_unittest("split_edges", test_split_edges), &
                  new_unittest("split_empty_input", test_split_empty_input), &
                  new_unittest("split_no_delimiter", test_split_no_delimiter), &
                  new_unittest("split_multichar_delimiter", test_split_multichar_delimiter), &
                  new_unittest("split_empty_delimiter", test_split_empty_delimiter), &
                  new_unittest("split_string_type_overloads", test_split_string_type_overloads), &
                  new_unittest("split_join_roundtrip", test_split_join_roundtrip), &
                  new_unittest("tokenize_basic", test_tokenize_basic), &
                  new_unittest("tokenize_whitespace_set", test_tokenize_whitespace_set), &
                  new_unittest("tokenize_empty", test_tokenize_empty), &
                  new_unittest("tokenize_string_type", test_tokenize_string_type), &
                  new_unittest("parse_int_valid", test_parse_int_valid), &
                  new_unittest("parse_int_whitespace", test_parse_int_whitespace), &
                  new_unittest("parse_int_boundaries", test_parse_int_boundaries), &
                  new_unittest("parse_int_errors", test_parse_int_errors), &
                  new_unittest("parse_int_overflow_32", test_parse_int_overflow_32), &
                  new_unittest("parse_int_overflow_64", test_parse_int_overflow_64), &
                  new_unittest("parse_int_string_type", test_parse_int_string_type), &
                  new_unittest("parse_real_valid", test_parse_real_valid), &
                  new_unittest("parse_real_exponents", test_parse_real_exponents), &
                  new_unittest("parse_real_errors", test_parse_real_errors), &
                  new_unittest("parse_real_rejects_list_directed", test_parse_real_rejects_list_directed), &
                  new_unittest("parse_real_overflow", test_parse_real_overflow), &
                  new_unittest("parse_real_single_precision", test_parse_real_single_precision), &
                  new_unittest("parse_real_string_type", test_parse_real_string_type), &
                  new_unittest("parse_tokens_of_a_line", test_parse_tokens_of_a_line) &
                  ]
   end subroutine collect_pic_tokenizer_tests

   !> Strict comparison of a string_type against a character literal:
   !> Fortran pads the shorter operand of `==` with blanks, which would hide
   !> exactly the empty field bugs these tests are looking for.
   function same(actual, expected) result(ok)
      type(string_type), intent(in) :: actual
      character(len=*), intent(in) :: expected
      logical :: ok
      character(len=:), allocatable :: raw

      raw = char(actual)
      ok = (len(raw) == len(expected))
      if (ok) ok = (raw == expected)
   end function same

   subroutine test_split_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("a,b,c", ",")
      call check(error, size(parts) == 3, "three fields expected")
      if (allocated(error)) return
      call check(error, same(parts(1), "a"), "first field should be a")
      if (allocated(error)) return
      call check(error, same(parts(2), "b"), "second field should be b")
      if (allocated(error)) return
      call check(error, same(parts(3), "c"), "third field should be c")
      if (allocated(error)) return

      parts = split("one word", " ")
      call check(error, size(parts) == 2, "two fields expected")
      if (allocated(error)) return
      call check(error, same(parts(1), "one"), "first field should be one")
   end subroutine test_split_basic

   subroutine test_split_empty_fields(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("a,,b", ",")
      call check(error, size(parts) == 3, "empty fields must be kept")
      if (allocated(error)) return
      call check(error, same(parts(1), "a"), "first field should be a")
      if (allocated(error)) return
      call check(error, same(parts(2), ""), "middle field should be empty")
      if (allocated(error)) return
      call check(error, same(parts(3), "b"), "third field should be b")
      if (allocated(error)) return

      parts = split(",,,", ",")
      call check(error, size(parts) == 4, "three delimiters give four empty fields")
      if (allocated(error)) return
      call check(error, same(parts(1), "") .and. same(parts(4), ""), "all fields empty")
   end subroutine test_split_empty_fields

   subroutine test_split_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split(",a", ",")
      call check(error, size(parts) == 2, "leading delimiter gives two fields")
      if (allocated(error)) return
      call check(error, same(parts(1), ""), "leading empty field")
      if (allocated(error)) return
      call check(error, same(parts(2), "a"), "second field should be a")
      if (allocated(error)) return

      parts = split("a,", ",")
      call check(error, size(parts) == 2, "trailing delimiter gives two fields")
      if (allocated(error)) return
      call check(error, same(parts(1), "a"), "first field should be a")
      if (allocated(error)) return
      call check(error, same(parts(2), ""), "trailing empty field")
      if (allocated(error)) return

      parts = split(",", ",")
      call check(error, size(parts) == 2, "a lone delimiter gives two empty fields")
      if (allocated(error)) return
      call check(error, same(parts(1), "") .and. same(parts(2), ""), "both fields empty")
   end subroutine test_split_edges

   subroutine test_split_empty_input(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("", ",")
      call check(error, size(parts) == 1, "empty input gives exactly one field")
      if (allocated(error)) return
      call check(error, same(parts(1), ""), "the single field is empty")
   end subroutine test_split_empty_input

   subroutine test_split_no_delimiter(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("abc", ",")
      call check(error, size(parts) == 1, "absent delimiter gives one field")
      if (allocated(error)) return
      call check(error, same(parts(1), "abc"), "the field is the whole string")
      if (allocated(error)) return

      parts = split("ab", "abcd")
      call check(error, size(parts) == 1, "delimiter longer than the string cannot match")
      if (allocated(error)) return
      call check(error, same(parts(1), "ab"), "the field is the whole string")
   end subroutine test_split_no_delimiter

   subroutine test_split_multichar_delimiter(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("a::b::c", "::")
      call check(error, size(parts) == 3, "three fields expected")
      if (allocated(error)) return
      call check(error, same(parts(2), "b"), "middle field should be b")
      if (allocated(error)) return

      parts = split("a:b", "::")
      call check(error, size(parts) == 1, "a partial match is not a delimiter")
      if (allocated(error)) return
      call check(error, same(parts(1), "a:b"), "the field is the whole string")
      if (allocated(error)) return

      parts = split("a:::b", "::")
      call check(error, size(parts) == 2, "matching is non overlapping and left to right")
      if (allocated(error)) return
      call check(error, same(parts(2), ":b"), "the leftover colon stays in the field")
   end subroutine test_split_multichar_delimiter

   subroutine test_split_empty_delimiter(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)

      parts = split("abc", "")
      call check(error, size(parts) == 1, "an empty delimiter cannot match")
      if (allocated(error)) return
      call check(error, same(parts(1), "abc"), "the field is the whole string")
   end subroutine test_split_empty_delimiter

   subroutine test_split_string_type_overloads(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: parts(:)
      type(string_type) :: text, delim

      text = string_type("a-b-c")
      delim = string_type("-")

      parts = split(text, "-")
      call check(error, size(parts) == 3 .and. same(parts(1), "a"), "string_type and char")
      if (allocated(error)) return

      parts = split("a-b-c", delim)
      call check(error, size(parts) == 3 .and. same(parts(2), "b"), "char and string_type")
      if (allocated(error)) return

      parts = split(text, delim)
      call check(error, size(parts) == 3 .and. same(parts(3), "c"), "string_type and string_type")
   end subroutine test_split_string_type_overloads

   subroutine test_split_join_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error

      call roundtrip(error, "a,b,c", ",")
      if (allocated(error)) return
      call roundtrip(error, "a,,b", ",")
      if (allocated(error)) return
      call roundtrip(error, ",a,", ",")
      if (allocated(error)) return
      call roundtrip(error, "abc", ",")
      if (allocated(error)) return
      call roundtrip(error, "", ",")
      if (allocated(error)) return
      call roundtrip(error, "a::b::c", "::")
   end subroutine test_split_join_roundtrip

   subroutine roundtrip(error, text, delim)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: text, delim
      character(len=:), allocatable :: rebuilt

      rebuilt = char(join(split(text, delim), delim))
      call check(error, rebuilt == text .and. len(rebuilt) == len(text), &
                 "split then join should rebuild '"//text//"'")
   end subroutine roundtrip

   subroutine test_tokenize_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: words(:)

      words = tokenize("  the quick   brown fox ")
      call check(error, size(words) == 4, "four words expected")
      if (allocated(error)) return
      call check(error, same(words(1), "the"), "first word")
      if (allocated(error)) return
      call check(error, same(words(3), "brown"), "third word")
      if (allocated(error)) return
      call check(error, same(words(4), "fox"), "last word")
      if (allocated(error)) return

      words = tokenize("solo")
      call check(error, size(words) == 1 .and. same(words(1), "solo"), "a single word")
   end subroutine test_tokenize_basic

   subroutine test_tokenize_whitespace_set(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: words(:)

      words = tokenize(TAB//"a"//LF//LF//"b"//CR//VT//"c"//FF)
      call check(error, size(words) == 3, "all whitespace characters separate")
      if (allocated(error)) return
      call check(error, same(words(1), "a"), "first word")
      if (allocated(error)) return
      call check(error, same(words(2), "b"), "second word")
      if (allocated(error)) return
      call check(error, same(words(3), "c"), "third word")
   end subroutine test_tokenize_whitespace_set

   subroutine test_tokenize_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: words(:)

      words = tokenize("")
      call check(error, size(words) == 0, "an empty string has no words")
      if (allocated(error)) return

      words = tokenize("   "//TAB//LF)
      call check(error, size(words) == 0, "an all whitespace string has no words")
   end subroutine test_tokenize_empty

   subroutine test_tokenize_string_type(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: words(:)

      words = tokenize(string_type(" alpha beta "))
      call check(error, size(words) == 2, "two words expected")
      if (allocated(error)) return
      call check(error, same(words(1), "alpha") .and. same(words(2), "beta"), "both words")
   end subroutine test_tokenize_string_type

   subroutine test_parse_int_valid(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: narrow
      integer(int64) :: wide
      type(error_t) :: err

      call parse_int("42", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 42_int32, "42 should parse")
      if (allocated(error)) return

      call parse_int("-42", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == -42_int32, "-42 should parse")
      if (allocated(error)) return

      call parse_int("+42", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 42_int32, "+42 should parse")
      if (allocated(error)) return

      call parse_int("0", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 0_int32, "0 should parse")
      if (allocated(error)) return

      call parse_int("-0", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 0_int32, "-0 should parse")
      if (allocated(error)) return

      call parse_int("007", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 7_int32, "leading zeros are fine")
      if (allocated(error)) return

      call parse_int("1234567890123", wide, err)
      call check(error,.not. err%has_error() .and. wide == 1234567890123_int64, "wide value")
   end subroutine test_parse_int_valid

   subroutine test_parse_int_whitespace(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: value
      type(error_t) :: err

      call parse_int("   42   ", value, err)
      call check(error,.not. err%has_error() .and. value == 42_int32, "surrounding blanks are stripped")
      if (allocated(error)) return

      call parse_int(TAB//"-7"//LF, value, err)
      call check(error,.not. err%has_error() .and. value == -7_int32, "tabs and newlines are stripped")
   end subroutine test_parse_int_whitespace

   subroutine test_parse_int_boundaries(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: narrow
      integer(int64) :: wide
      type(error_t) :: err

      call parse_int("2147483647", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == huge(narrow), "int32 maximum")
      if (allocated(error)) return

      call parse_int("-2147483648", narrow, err)
      call check(error,.not. err%has_error() .and. narrow == -huge(narrow) - 1_int32, "int32 minimum")
      if (allocated(error)) return

      call parse_int("9223372036854775807", wide, err)
      call check(error,.not. err%has_error() .and. wide == huge(wide), "int64 maximum")
      if (allocated(error)) return

      call parse_int("-9223372036854775808", wide, err)
      call check(error,.not. err%has_error() .and. wide == -huge(wide) - 1_int64, "int64 minimum")
   end subroutine test_parse_int_boundaries

   subroutine test_parse_int_errors(error)
      type(error_type), allocatable, intent(out) :: error

      call expect_int_error(error, "")
      if (allocated(error)) return
      call expect_int_error(error, "    ")
      if (allocated(error)) return
      call expect_int_error(error, "abc")
      if (allocated(error)) return
      call expect_int_error(error, "12abc")
      if (allocated(error)) return
      call expect_int_error(error, "1 2")
      if (allocated(error)) return
      call expect_int_error(error, "-")
      if (allocated(error)) return
      call expect_int_error(error, "+")
      if (allocated(error)) return
      call expect_int_error(error, "1.5")
      if (allocated(error)) return
      call expect_int_error(error, "12-")
      if (allocated(error)) return
      call expect_int_error(error, "--3")
   end subroutine test_parse_int_errors

   subroutine test_parse_int_overflow_32(error)
      type(error_type), allocatable, intent(out) :: error

      call expect_int_error(error, "2147483648")
      if (allocated(error)) return
      call expect_int_error(error, "-2147483649")
      if (allocated(error)) return
      call expect_int_error(error, "3000000000")
   end subroutine test_parse_int_overflow_32

   subroutine test_parse_int_overflow_64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: value
      type(error_t) :: err

      ! One past the int64 maximum: caught by the "limit + digit" check.
      call parse_int("9223372036854775808", value, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "int64 overflow by one")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0 .and. value == 0_int64, "message set, value zeroed")
      if (allocated(error)) return

      ! Far past the maximum: caught by the multiplication guard instead.
      call parse_int("99999999999999999999", value, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "int64 overflow by orders")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0 .and. value == 0_int64, "message set, value zeroed")
      if (allocated(error)) return

      call parse_int("-99999999999999999999", value, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "negative int64 overflow")
      if (allocated(error)) return
      call check(error, value == 0_int64, "value zeroed")
   end subroutine test_parse_int_overflow_64

   subroutine test_parse_int_string_type(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: narrow
      integer(int64) :: wide
      type(error_t) :: err

      call parse_int(string_type(" 314 "), narrow, err)
      call check(error,.not. err%has_error() .and. narrow == 314_int32, "string_type into int32")
      if (allocated(error)) return

      call parse_int(string_type("-314"), wide, err)
      call check(error,.not. err%has_error() .and. wide == -314_int64, "string_type into int64")
      if (allocated(error)) return

      call parse_int(string_type("nope"), narrow, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "string_type garbage")
      if (allocated(error)) return

      call parse_int(string_type("nope"), wide, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "string_type garbage, wide")
   end subroutine test_parse_int_string_type

   subroutine expect_int_error(error, text)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: text
      integer(int32) :: narrow
      integer(int64) :: wide
      type(error_t) :: err

      call parse_int(text, narrow, err)
      call check(error, err%has_error(), "expected a failure for '"//text//"'")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_PARSE, "expected ERROR_PARSE for '"//text//"'")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0, "expected a message for '"//text//"'")
      if (allocated(error)) return
      call check(error, narrow == 0_int32, "expected a zeroed value for '"//text//"'")
      if (allocated(error)) return

      ! The same input must fail the same way through the int64 procedure,
      ! which is what integer(default_int) resolves to in the USE_INT8 build.
      call parse_int(text, wide, err)
      if (text == "2147483648" .or. text == "-2147483649" .or. text == "3000000000") return
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, &
                 "expected ERROR_PARSE from the int64 path for '"//text//"'")
      if (allocated(error)) return
      call check(error, wide == 0_int64, "expected a zeroed wide value for '"//text//"'")
   end subroutine expect_int_error

   subroutine test_parse_real_valid(error)
      type(error_type), allocatable, intent(out) :: error

      call expect_real(error, "1.5", 1.5_dp)
      if (allocated(error)) return
      call expect_real(error, "-1.5", -1.5_dp)
      if (allocated(error)) return
      call expect_real(error, "+1.5", 1.5_dp)
      if (allocated(error)) return
      call expect_real(error, "3", 3.0_dp)
      if (allocated(error)) return
      call expect_real(error, "-7", -7.0_dp)
      if (allocated(error)) return
      call expect_real(error, ".5", 0.5_dp)
      if (allocated(error)) return
      call expect_real(error, "-.5", -0.5_dp)
      if (allocated(error)) return
      call expect_real(error, "5.", 5.0_dp)
      if (allocated(error)) return
      call expect_real(error, "0.0", 0.0_dp)
      if (allocated(error)) return
      call expect_real(error, "   2.25   ", 2.25_dp)
      if (allocated(error)) return
      call expect_real(error, TAB//"2.25"//LF, 2.25_dp)
   end subroutine test_parse_real_valid

   subroutine test_parse_real_exponents(error)
      type(error_type), allocatable, intent(out) :: error

      call expect_real(error, "1e3", 1000.0_dp)
      if (allocated(error)) return
      call expect_real(error, "1E3", 1000.0_dp)
      if (allocated(error)) return
      call expect_real(error, "1d3", 1000.0_dp)
      if (allocated(error)) return
      call expect_real(error, "1D3", 1000.0_dp)
      if (allocated(error)) return
      call expect_real(error, "1e+3", 1000.0_dp)
      if (allocated(error)) return
      call expect_real(error, "1.5e-3", 0.0015_dp)
      if (allocated(error)) return
      call expect_real(error, ".5e1", 5.0_dp)
      if (allocated(error)) return
      call expect_real(error, "-2.5E2", -250.0_dp)
   end subroutine test_parse_real_exponents

   subroutine test_parse_real_errors(error)
      type(error_type), allocatable, intent(out) :: error

      call expect_real_error(error, "")
      if (allocated(error)) return
      call expect_real_error(error, "   ")
      if (allocated(error)) return
      call expect_real_error(error, "abc")
      if (allocated(error)) return
      call expect_real_error(error, ".")
      if (allocated(error)) return
      call expect_real_error(error, "+.")
      if (allocated(error)) return
      call expect_real_error(error, "-")
      if (allocated(error)) return
      call expect_real_error(error, "12abc")
      if (allocated(error)) return
      call expect_real_error(error, "1.2.3")
      if (allocated(error)) return
      call expect_real_error(error, "1e")
      if (allocated(error)) return
      call expect_real_error(error, "1e+")
      if (allocated(error)) return
      call expect_real_error(error, "1e2x")
      if (allocated(error)) return
      call expect_real_error(error, "1x2")
      if (allocated(error)) return
      call expect_real_error(error, "1 2")
      if (allocated(error)) return
      call expect_real_error(error, "1q3")
   end subroutine test_parse_real_errors

   subroutine test_parse_real_rejects_list_directed(error)
      type(error_type), allocatable, intent(out) :: error

      ! Every one of these is silently accepted by a list directed internal
      ! read, which is exactly why the parser does not use one.
      call expect_real_error(error, "1,2")
      if (allocated(error)) return
      call expect_real_error(error, "3*7")
      if (allocated(error)) return
      call expect_real_error(error, "nan")
      if (allocated(error)) return
      call expect_real_error(error, "NaN")
      if (allocated(error)) return
      call expect_real_error(error, "inf")
      if (allocated(error)) return
      call expect_real_error(error, "-Infinity")
      if (allocated(error)) return
      call expect_real_error(error, "1.5 junk")
   end subroutine test_parse_real_rejects_list_directed

   subroutine test_parse_real_overflow(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: value
      type(error_t) :: err

      call parse_real("1e999", value, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "1e999 overflows dp")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0 .and. value == 0.0_dp, "message set, value zeroed")
      if (allocated(error)) return

      call parse_real("-1e999", value, err)
      call check(error, err%has_error(), "-1e999 overflows dp")
      if (allocated(error)) return

      ! Underflow is not an error, it simply rounds to zero.
      call parse_real("1e-999", value, err)
      call check(error,.not. err%has_error() .and. value == 0.0_dp, "underflow rounds to zero")
   end subroutine test_parse_real_overflow

   subroutine test_parse_real_single_precision(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: value
      type(error_t) :: err

      call parse_real("1.5", value, err)
      call check(error,.not. err%has_error() .and. abs(value - 1.5_sp) < 1.0e-6_sp, "1.5 into sp")
      if (allocated(error)) return

      call parse_real("-2.5e2", value, err)
      call check(error,.not. err%has_error() .and. abs(value + 250.0_sp) < 1.0e-3_sp, "-2.5e2 into sp")
      if (allocated(error)) return

      call parse_real("1e300", value, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "1e300 overflows sp")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0 .and. value == 0.0_sp, "message set, value zeroed")
      if (allocated(error)) return

      call parse_real("bad", value, err)
      call check(error, err%has_error() .and. value == 0.0_sp, "garbage into sp")
   end subroutine test_parse_real_single_precision

   subroutine test_parse_real_string_type(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: wide
      real(sp) :: narrow
      type(error_t) :: err

      call parse_real(string_type(" 1.25 "), wide, err)
      call check(error,.not. err%has_error() .and. abs(wide - 1.25_dp) < 1.0e-12_dp, "string_type into dp")
      if (allocated(error)) return

      call parse_real(string_type("1.25"), narrow, err)
      call check(error,.not. err%has_error() .and. abs(narrow - 1.25_sp) < 1.0e-6_sp, "string_type into sp")
      if (allocated(error)) return

      call parse_real(string_type("oops"), wide, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "string_type garbage, dp")
      if (allocated(error)) return

      call parse_real(string_type("oops"), narrow, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "string_type garbage, sp")
   end subroutine test_parse_real_string_type

   subroutine expect_real(error, text, expected)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: text
      real(dp), intent(in) :: expected
      real(dp) :: value
      type(error_t) :: err

      call parse_real(text, value, err)
      call check(error,.not. err%has_error(), "'"//text//"' should parse cleanly")
      if (allocated(error)) return
      call check(error, abs(value - expected) <= 1.0e-12_dp*max(1.0_dp, abs(expected)), &
                 "wrong value for '"//text//"'")
   end subroutine expect_real

   subroutine expect_real_error(error, text)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: text
      real(dp) :: value
      type(error_t) :: err

      call parse_real(text, value, err)
      call check(error, err%has_error(), "expected a failure for '"//text//"'")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_PARSE, "expected ERROR_PARSE for '"//text//"'")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0, "expected a message for '"//text//"'")
      if (allocated(error)) return
      call check(error, value == 0.0_dp, "expected a zeroed value for '"//text//"'")
   end subroutine expect_real_error

   subroutine test_parse_tokens_of_a_line(error)
      !! The intended end to end use: tokenize a line, then parse the fields.
      type(error_type), allocatable, intent(out) :: error
      type(string_type), allocatable :: fields(:)
      real(dp) :: x, y, z
      integer(int32) :: label
      type(error_t) :: err

      fields = tokenize("  8   0.0 -1.25   3.5e1  ")
      call check(error, size(fields) == 4, "four fields on the line")
      if (allocated(error)) return

      call parse_int(fields(1), label, err)
      call check(error,.not. err%has_error() .and. label == 8_int32, "the label parses")
      if (allocated(error)) return

      call parse_real(fields(2), x, err)
      if (.not. err%has_error()) call parse_real(fields(3), y, err)
      if (.not. err%has_error()) call parse_real(fields(4), z, err)
      call check(error,.not. err%has_error(), "the coordinates parse")
      if (allocated(error)) return
      call check(error, abs(x) < 1.0e-12_dp .and. abs(y + 1.25_dp) < 1.0e-12_dp &
                 .and. abs(z - 35.0_dp) < 1.0e-12_dp, "the coordinate values are right")
      if (allocated(error)) return

      ! A CSV style record keeps its empty field, so column three stays column three.
      fields = split("8,,3.5", ",")
      call check(error, size(fields) == 3, "three columns")
      if (allocated(error)) return
      call parse_real(fields(2), x, err)
      call check(error, err%has_error() .and. err%get_code() == ERROR_PARSE, "the empty column is reported")
   end subroutine test_parse_tokens_of_a_line

end module test_pic_tokenizer
