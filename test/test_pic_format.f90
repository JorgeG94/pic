! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Tests for pic_format.
!!
!! Every assertion here is an exact string comparison. That is the whole point
!! of the module: a tolerance based test would not catch the compiler to
!! compiler differences the module exists to eliminate.
module test_pic_format
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, sp, dp
   use pic_format, only: to_string_fixed, to_string_sci, to_string_width, &
                         PIC_FORMAT_NAN, PIC_FORMAT_INF, PIC_FORMAT_NEG_INF, &
                         PIC_FORMAT_MAX_DECIMALS, PIC_FORMAT_MAX_SIG_DIGITS
   implicit none
   private

   public :: collect_pic_format_tests

contains

   subroutine collect_pic_format_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("fixed_basic", test_fixed_basic), &
                  new_unittest("fixed_half_even_ties", test_fixed_half_even_ties), &
                  new_unittest("fixed_decimal_boundaries", test_fixed_decimal_boundaries), &
                  new_unittest("fixed_below_one", test_fixed_below_one), &
                  new_unittest("fixed_negative", test_fixed_negative), &
                  new_unittest("fixed_zero_and_negative_zero", test_fixed_zero_and_negative_zero), &
                  new_unittest("fixed_zero_decimals", test_fixed_zero_decimals), &
                  new_unittest("fixed_carry_out", test_fixed_carry_out), &
                  new_unittest("fixed_extremes", test_fixed_extremes), &
                  new_unittest("fixed_clamping", test_fixed_clamping), &
                  new_unittest("fixed_specials", test_fixed_specials), &
                  new_unittest("fixed_single_precision", test_fixed_single_precision), &
                  new_unittest("sci_basic", test_sci_basic), &
                  new_unittest("sci_exponent_field", test_sci_exponent_field), &
                  new_unittest("sci_rounding", test_sci_rounding), &
                  new_unittest("sci_zero_and_negative_zero", test_sci_zero_and_negative_zero), &
                  new_unittest("sci_clamping", test_sci_clamping), &
                  new_unittest("sci_specials", test_sci_specials), &
                  new_unittest("sci_single_precision", test_sci_single_precision), &
                  new_unittest("width_int32", test_width_int32), &
                  new_unittest("width_int64", test_width_int64), &
                  new_unittest("width_edges", test_width_edges), &
                  new_unittest("exported_constants", test_exported_constants) &
                  ]

   end subroutine collect_pic_format_tests

   subroutine test_fixed_basic(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(1.0_dp/3.0_dp, 6_default_int) == "0.333333", &
                 "one third to six decimals")
      if (allocated(error)) return
      call check(error, to_string_fixed(2.0_dp/3.0_dp, 6_default_int) == "0.666667", &
                 "two thirds to six decimals")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.0_dp, 6_default_int) == "1.000000", &
                 "one to six decimals")
      if (allocated(error)) return
      call check(error, to_string_fixed(123.456_dp, 2_default_int) == "123.46", &
                 "rounding up a normal value")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.001_dp, 6_default_int) == "0.001000", &
                 "trailing zeros are padded out")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.5_dp, 5_default_int) == "0.50000", &
                 "exact value shorter than the requested precision")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.0_dp/3.0_dp, 20_default_int) == "0.33333333333333331483", &
                 "the exact binary value shows through past 17 digits")

   end subroutine test_fixed_basic

   subroutine test_fixed_half_even_ties(error)
      !! Exact ties, the only inputs where the half to even rule is visible.
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(0.5_dp, 0_default_int) == "0", "0.5 ties down to even 0")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.5_dp, 0_default_int) == "2", "1.5 ties up to even 2")
      if (allocated(error)) return
      call check(error, to_string_fixed(2.5_dp, 0_default_int) == "2", "2.5 ties down to even 2")
      if (allocated(error)) return
      call check(error, to_string_fixed(3.5_dp, 0_default_int) == "4", "3.5 ties up to even 4")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.25_dp, 1_default_int) == "0.2", "0.25 ties down to even 2")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.125_dp, 2_default_int) == "0.12", "0.125 ties down to even 2")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.375_dp, 2_default_int) == "0.38", "0.375 ties up to even 8")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.0625_dp, 3_default_int) == "0.062", "0.0625 ties down to even 2")

   end subroutine test_fixed_half_even_ties

   subroutine test_fixed_decimal_boundaries(error)
      !! Decimal literals that only look like ties. The stored double is never
      !! exactly halfway, so the exact expansion decides and the answer is not
      !! the one a naive half up rule on the literal would give.
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(0.05_dp, 1_default_int) == "0.1", &
                 "0.05 is stored just above the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.15_dp, 1_default_int) == "0.1", &
                 "0.15 is stored just below the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.35_dp, 1_default_int) == "0.3", &
                 "0.35 is stored just below the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.45_dp, 1_default_int) == "0.5", &
                 "0.45 is stored just above the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.05_dp, 2_default_int) == "0.05", &
                 "0.05 at its own precision")
      if (allocated(error)) return
      call check(error, to_string_fixed(2.675_dp, 2_default_int) == "2.67", &
                 "2.675 is stored just below the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.005_dp, 2_default_int) == "1.00", &
                 "1.005 is stored just below the tie")
      if (allocated(error)) return
      call check(error, to_string_fixed(9.995_dp, 2_default_int) == "9.99", &
                 "9.995 is stored just below the tie")

   end subroutine test_fixed_decimal_boundaries

   subroutine test_fixed_below_one(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(0.1_dp, 1_default_int) == "0.1", "leading zero is always written")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.1_dp, 20_default_int) == "0.10000000000000000555", &
                 "exact expansion of one tenth")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.0e-9_dp, 6_default_int) == "0.000000", &
                 "a value far below the last decimal rounds to zero")
      if (allocated(error)) return
      call check(error, to_string_fixed(2.2250738585072014e-308_dp, 6_default_int) == "0.000000", &
                 "smallest normal double rounds to zero")

   end subroutine test_fixed_below_one

   subroutine test_fixed_negative(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(-123.456_dp, 2_default_int) == "-123.46", "negative value")
      if (allocated(error)) return
      call check(error, to_string_fixed(-0.5_dp, 0_default_int) == "-0", "negative tie down keeps its sign")
      if (allocated(error)) return
      call check(error, to_string_fixed(-1.5_dp, 0_default_int) == "-2", "negative tie up")
      if (allocated(error)) return
      call check(error, to_string_fixed(-2.5_dp, 0_default_int) == "-2", "negative tie down to even")
      if (allocated(error)) return
      call check(error, to_string_fixed(-1.0e-9_dp, 6_default_int) == "-0.000000", &
                 "a negative value that rounds to zero keeps its sign")
      if (allocated(error)) return
      call check(error, to_string_fixed(-0.9999_dp, 3_default_int) == "-1.000", &
                 "negative value that carries out")

   end subroutine test_fixed_negative

   subroutine test_fixed_zero_and_negative_zero(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: negative_zero

      negative_zero = sign(0.0_dp, -1.0_dp)

      call check(error, to_string_fixed(0.0_dp, 3_default_int) == "0.000", "positive zero")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.0_dp, 0_default_int) == "0", "positive zero, no decimals")
      if (allocated(error)) return
      call check(error, to_string_fixed(negative_zero, 2_default_int) == "-0.00", "negative zero keeps its sign")
      if (allocated(error)) return
      call check(error, to_string_fixed(negative_zero, 0_default_int) == "-0", &
                 "negative zero with no decimals")

   end subroutine test_fixed_zero_and_negative_zero

   subroutine test_fixed_zero_decimals(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(1.0_dp, 0_default_int) == "1", "no trailing decimal point")
      if (allocated(error)) return
      call check(error, to_string_fixed(1024.0_dp, 0_default_int) == "1024", "power of two, no decimals")
      if (allocated(error)) return
      call check(error, to_string_fixed(123.456_dp, 0_default_int) == "123", "truncating to units rounds down")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.6_dp, 0_default_int) == "1", "rounds up to one")

   end subroutine test_fixed_zero_decimals

   subroutine test_fixed_carry_out(error)
      !! Rounding that propagates a carry past the leading digit.
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(9.999_dp, 2_default_int) == "10.00", "9.999 carries into a new digit")
      if (allocated(error)) return
      call check(error, to_string_fixed(99.995_dp, 2_default_int) == "100.00", "99.995 carries twice")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.9999_dp, 3_default_int) == "1.000", "carry moves the point past zero")

   end subroutine test_fixed_carry_out

   subroutine test_fixed_extremes(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: rendered

      rendered = to_string_fixed(2.0_dp**52, 0_default_int)
      call check(error, rendered == "4503599627370496", "two to the fifty second is exact")
      if (allocated(error)) return

      rendered = to_string_fixed(1.0e20_dp, 0_default_int)
      call check(error, rendered == "100000000000000000000", "ten to the twentieth is exact")
      if (allocated(error)) return

      rendered = to_string_fixed(huge(1.0_dp), 0_default_int)
      call check(error, len(rendered) == 309, "largest double has 309 integer digits")
      if (allocated(error)) return
      call check(error, rendered(1:20) == "17976931348623157081", "leading digits of the largest double")
      if (allocated(error)) return
      call check(error, rendered(300:309) == "4124858368", "trailing digits of the largest double")
      if (allocated(error)) return

      rendered = to_string_fixed(tiny(1.0_dp), 320_default_int)
      call check(error, len(rendered) == 202, "decimals are clamped even for tiny values")

   end subroutine test_fixed_extremes

   subroutine test_fixed_clamping(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: rendered

      call check(error, to_string_fixed(1.23456_dp, -3_default_int) == "1", &
                 "negative decimals clamp to zero")
      if (allocated(error)) return

      rendered = to_string_fixed(1.0_dp, 500_default_int)
      call check(error, len(rendered) == 202, "decimals clamp to the documented maximum")
      if (allocated(error)) return
      call check(error, rendered(1:3) == "1.0", "clamped result still starts correctly")
      if (allocated(error)) return
      call check(error, rendered(202:202) == "0", "clamped result is zero filled")

   end subroutine test_fixed_clamping

   subroutine test_fixed_specials(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: not_a_number, plus_inf, minus_inf

      call make_specials(not_a_number, plus_inf, minus_inf)

      call check(error, to_string_fixed(not_a_number, 6_default_int) == "NaN", "NaN spelling")
      if (allocated(error)) return
      call check(error, to_string_fixed(-not_a_number, 6_default_int) == "NaN", "negative NaN spells the same")
      if (allocated(error)) return
      call check(error, to_string_fixed(plus_inf, 6_default_int) == "Inf", "positive infinity spelling")
      if (allocated(error)) return
      call check(error, to_string_fixed(minus_inf, 6_default_int) == "-Inf", "negative infinity spelling")

   end subroutine test_fixed_specials

   subroutine test_fixed_single_precision(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_fixed(0.5_sp, 1_default_int) == "0.5", "single precision exact half")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.25_sp, 1_default_int) == "0.2", "single precision tie to even")
      if (allocated(error)) return
      call check(error, to_string_fixed(2.5_sp, 0_default_int) == "2", "single precision tie to even at units")
      if (allocated(error)) return
      call check(error, to_string_fixed(1.0_sp/3.0_sp, 6_default_int) == "0.333333", "single precision one third")
      if (allocated(error)) return
      call check(error, to_string_fixed(0.1_sp, 10_default_int) == "0.1000000015", &
                 "single precision one tenth shows its exact value")

   end subroutine test_fixed_single_precision

   subroutine test_sci_basic(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_sci(1.0e5_dp, 7_default_int) == "1.000000e+05", &
                 "the canonical rendering of 1.0e5")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0_dp, 6_default_int) == "1.00000e+00", "one")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0_dp, 1_default_int) == "1e+00", "one significant digit has no point")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0_dp/3.0_dp, 6_default_int) == "3.33333e-01", "one third")
      if (allocated(error)) return
      call check(error, to_string_sci(2.0_dp/3.0_dp, 6_default_int) == "6.66667e-01", "two thirds")
      if (allocated(error)) return
      call check(error, to_string_sci(6.02214076e23_dp, 9_default_int) == "6.02214076e+23", "a large constant")
      if (allocated(error)) return
      call check(error, to_string_sci(123456.0_dp, 3_default_int) == "1.23e+05", "three significant digits")
      if (allocated(error)) return
      call check(error, to_string_sci(-2.5e-7_dp, 4_default_int) == "-2.500e-07", "a small negative value")

   end subroutine test_sci_basic

   subroutine test_sci_exponent_field(error)
      !! The exponent field is the single largest source of divergence between
      !! compilers, so it gets its own test.
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_sci(1.0e-5_dp, 6_default_int) == "1.00000e-05", &
                 "single digit exponents are zero padded to two")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0e-300_dp, 3_default_int) == "1.00e-300", &
                 "three digit negative exponent")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0e300_dp, 3_default_int) == "1.00e+300", &
                 "three digit positive exponent")
      if (allocated(error)) return
      call check(error, to_string_sci(huge(1.0_dp), 3_default_int) == "1.80e+308", &
                 "largest double")
      if (allocated(error)) return
      call check(error, to_string_sci(tiny(1.0_dp), 3_default_int) == "2.23e-308", &
                 "smallest normal double")

   end subroutine test_sci_exponent_field

   subroutine test_sci_rounding(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_sci(999.9_dp, 3_default_int) == "1.00e+03", &
                 "carry out bumps the exponent")
      if (allocated(error)) return
      call check(error, to_string_sci(999.9_dp, 2_default_int) == "1.0e+03", &
                 "carry out with two significant digits")
      if (allocated(error)) return
      call check(error, to_string_sci(0.25_dp, 1_default_int) == "2e-01", "tie to even")
      if (allocated(error)) return
      call check(error, to_string_sci(1.5_dp, 1_default_int) == "2e+00", "tie up to even")
      if (allocated(error)) return
      call check(error, to_string_sci(2.5_dp, 1_default_int) == "2e+00", "tie down to even")
      if (allocated(error)) return
      call check(error, to_string_sci(0.35_dp, 1_default_int) == "3e-01", "just below a tie")

   end subroutine test_sci_rounding

   subroutine test_sci_zero_and_negative_zero(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: negative_zero

      negative_zero = sign(0.0_dp, -1.0_dp)

      call check(error, to_string_sci(0.0_dp, 6_default_int) == "0.00000e+00", "zero has a zero exponent")
      if (allocated(error)) return
      call check(error, to_string_sci(0.0_dp, 1_default_int) == "0e+00", "zero with one significant digit")
      if (allocated(error)) return
      call check(error, to_string_sci(negative_zero, 6_default_int) == "-0.00000e+00", &
                 "negative zero keeps its sign")

   end subroutine test_sci_zero_and_negative_zero

   subroutine test_sci_clamping(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: rendered

      call check(error, to_string_sci(1.0_dp, 0_default_int) == "1e+00", &
                 "zero significant digits clamps to one")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0_dp, -7_default_int) == "1e+00", &
                 "negative significant digits clamps to one")
      if (allocated(error)) return

      rendered = to_string_sci(1.0_dp/3.0_dp, 100_default_int)
      call check(error, rendered == "3.333333333333333148296162562473909929395e-01", &
                 "significant digits clamp to the documented maximum")

   end subroutine test_sci_clamping

   subroutine test_sci_specials(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: not_a_number, plus_inf, minus_inf

      call make_specials(not_a_number, plus_inf, minus_inf)

      call check(error, to_string_sci(not_a_number, 6_default_int) == "NaN", "NaN spelling")
      if (allocated(error)) return
      call check(error, to_string_sci(plus_inf, 6_default_int) == "Inf", "positive infinity spelling")
      if (allocated(error)) return
      call check(error, to_string_sci(minus_inf, 6_default_int) == "-Inf", "negative infinity spelling")

   end subroutine test_sci_specials

   subroutine test_sci_single_precision(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_sci(1.0_sp, 3_default_int) == "1.00e+00", "single precision one")
      if (allocated(error)) return
      call check(error, to_string_sci(1.0_sp/3.0_sp, 9_default_int) == "3.33333343e-01", &
                 "single precision one third")
      if (allocated(error)) return
      call check(error, to_string_sci(0.1_sp, 9_default_int) == "1.00000001e-01", &
                 "single precision one tenth")

   end subroutine test_sci_single_precision

   subroutine test_width_int32(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_width(42_int32, 6_default_int) == "    42", "padded to six")
      if (allocated(error)) return
      call check(error, to_string_width(123456_int32, 6_default_int) == "123456", "fits exactly")
      if (allocated(error)) return
      call check(error, to_string_width(1234567_int32, 6_default_int) == "1234567", &
                 "one too narrow widens instead of truncating")
      if (allocated(error)) return
      call check(error, to_string_width(-42_int32, 6_default_int) == "   -42", "the sign counts toward the width")
      if (allocated(error)) return
      call check(error, to_string_width(-123456_int32, 6_default_int) == "-123456", &
                 "a sign that does not fit widens the field")
      if (allocated(error)) return
      call check(error, to_string_width(0_int32, 3_default_int) == "  0", "zero")
      if (allocated(error)) return
      call check(error, to_string_width(huge(1_int32), 12_default_int) == "  2147483647", "largest int32")
      if (allocated(error)) return
      call check(error, to_string_width(-huge(1_int32) - 1_int32, 12_default_int) == " -2147483648", &
                 "most negative int32 does not overflow the conversion")

   end subroutine test_width_int32

   subroutine test_width_int64(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_width(42_int64, 6_default_int) == "    42", "padded to six")
      if (allocated(error)) return
      call check(error, to_string_width(0_int64, 1_default_int) == "0", "zero fits exactly")
      if (allocated(error)) return
      call check(error, to_string_width(huge(1_int64), 22_default_int) == "   9223372036854775807", &
                 "largest int64")
      if (allocated(error)) return
      call check(error, to_string_width(-huge(1_int64) - 1_int64, 22_default_int) == "  -9223372036854775808", &
                 "most negative int64 does not overflow the conversion")
      if (allocated(error)) return
      call check(error, to_string_width(-huge(1_int64) - 1_int64, 4_default_int) == "-9223372036854775808", &
                 "most negative int64 widens a narrow field")

   end subroutine test_width_int64

   subroutine test_width_edges(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string_width(7_int32, 0_default_int) == "7", "zero width is the bare number")
      if (allocated(error)) return
      call check(error, to_string_width(7_int32, -5_default_int) == "7", "negative width is the bare number")
      if (allocated(error)) return
      call check(error, to_string_width(7_int32, 1_default_int) == "7", "width equal to the length")
      if (allocated(error)) return
      call check(error, len(to_string_width(7_int32, 9_default_int)) == 9, "result length is the width")

   end subroutine test_width_edges

   subroutine test_exported_constants(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, PIC_FORMAT_NAN == "NaN", "NaN constant")
      if (allocated(error)) return
      call check(error, PIC_FORMAT_INF == "Inf", "Inf constant")
      if (allocated(error)) return
      call check(error, PIC_FORMAT_NEG_INF == "-Inf", "negative Inf constant")
      if (allocated(error)) return
      call check(error, PIC_FORMAT_MAX_DECIMALS == 200_default_int, "maximum decimals")
      if (allocated(error)) return
      call check(error, PIC_FORMAT_MAX_SIG_DIGITS == 40_default_int, "maximum significant digits")

   end subroutine test_exported_constants

   subroutine make_specials(not_a_number, plus_inf, minus_inf)
      !! Build the IEEE special values without arithmetic, so that the test
      !! does not trip the -ffpe-trap=invalid,zero,overflow debug build.
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
                                                                                  ieee_positive_inf, ieee_negative_inf
      real(dp), intent(out) :: not_a_number, plus_inf, minus_inf

      not_a_number = ieee_value(1.0_dp, ieee_quiet_nan)
      plus_inf = ieee_value(1.0_dp, ieee_positive_inf)
      minus_inf = ieee_value(1.0_dp, ieee_negative_inf)

   end subroutine make_specials

end module test_pic_format
