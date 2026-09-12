! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Deterministic number-to-string formatting.
!!
!! Every routine in this module produces a byte-identical result on every
!! supported compiler for the same input value. That guarantee is what makes
!! golden-file testing possible in Fortran: list-directed output and the `g0`
!! edit descriptor are explicitly processor dependent, so GNU, Intel, NVIDIA
!! HPC and LFortran disagree about digit counts, trailing zeros, the presence
!! of a leading zero before the decimal point, the exponent letter and the
!! exponent field width.
module pic_format
   !! Deterministic, compiler independent formatting of reals and integers.
   !!
   !! ### Why this module contains no write statement
   !!
   !! Not even an internal `write` with a fully explicit edit descriptor is used
   !! for reals, because two things remain processor dependent even then:
   !!
   !! * the rounding mode applied to the last emitted digit (`ROUND=` defaults
   !!   to `PROCESSOR_DEFINED`, and the `ROUND=` specifier itself is not
   !!   uniformly supported), and
   !! * whether an optional leading zero is written for `Fw.d` output of a
   !!   magnitude below one (`0.25` versus `.25`).
   !!
   !! Instead the exact decimal expansion of the IEEE value is generated here
   !! with integer arithmetic only, and then rounded explicitly. Integers are
   !! delegated to `to_string` from `pic_strings`, whose no-format integer path
   !! is already a hand rolled digit by digit conversion that accumulates in
   !! negative space so that `-huge()-1` converts without overflow.
   !!
   !! ### Rounding
   !!
   !! Reals are rounded round half to even against the *exact* binary value of
   !! the argument, not against the decimal literal that was written in the
   !! source. This is the only rounding rule that can be reproduced exactly, and
   !! it is occasionally surprising: `0.15_dp` is really
   !! `0.1499999999999999944...`, so `to_string_fixed(0.15_dp, 1)` is `"0.1"`,
   !! not `"0.2"`. A tie is only a tie when the binary value is exactly halfway,
   !! which happens only for dyadic rationals such as `0.25` or `2.5`.
   !!
   !! ### Special values
   !!
   !! Canonical spellings, identical on every compiler and in every routine:
   !! NaN of any payload or sign renders as `NaN`, positive infinity as `Inf`
   !! and negative infinity as `-Inf`.
   !!
   !! The sign of a non-zero argument is always preserved, including for values
   !! that round to zero: `to_string_fixed(-1.0e-9_dp, 2)` is `"-0.00"`.
   !!
   !! Zero is always rendered without a sign, so `to_string_fixed(-0.0_dp, 2)`
   !! is `"0.00"`, not `"-0.00"`. Distinguishing the signed zeros is processor
   !! dependent in Fortran (F2018 16.9.165 makes SIGN return `|A|` where the
   !! processor cannot tell them apart) and Intel and AOCC do not tell them
   !! apart under their default floating point model. Rendering a sign here
   !! would make the output depend on the compiler and its flags, which is
   !! exactly what this module exists to prevent, so negative zero is
   !! canonicalised to positive zero instead. This matches `pic_array_hash`,
   !! which collapses the signed zeros to one digest for the same reason.
   use pic_types, only: default_int, int32, int64, sp, dp
   use pic_strings, only: to_string
   implicit none
   private

   public :: to_string_fixed
   public :: to_string_sci
   public :: to_string_width
   public :: PIC_FORMAT_NAN
   public :: PIC_FORMAT_INF
   public :: PIC_FORMAT_NEG_INF
   public :: PIC_FORMAT_MAX_DECIMALS
   public :: PIC_FORMAT_MAX_SIG_DIGITS

   character(len=*), parameter :: PIC_FORMAT_NAN = "NaN"
      !! Canonical rendering of a not a number value.
   character(len=*), parameter :: PIC_FORMAT_INF = "Inf"
      !! Canonical rendering of positive infinity.
   character(len=*), parameter :: PIC_FORMAT_NEG_INF = "-Inf"
      !! Canonical rendering of negative infinity.

   integer(default_int), parameter :: PIC_FORMAT_MAX_DECIMALS = 200_default_int
      !! Largest number of decimals honoured by `to_string_fixed`; larger
      !! requests are clamped to this value so that the output stays bounded.
   integer(default_int), parameter :: PIC_FORMAT_MAX_SIG_DIGITS = 40_default_int
      !! Largest number of significant digits honoured by `to_string_sci`;
      !! larger requests are clamped to this value.

   integer(default_int), parameter :: LIMB_DIGITS = 9_default_int
      !! Decimal digits carried in one big integer limb.
   integer(int64), parameter :: LIMB_BASE = 1000000000_int64
      !! Radix of the big integer scratch representation. Fixed width `int64`
      !! on purpose: the limb products must not change size with the build
      !! default integer kind.
   integer(default_int), parameter :: MAX_LIMBS = 100_default_int
      !! Limb capacity. The widest intermediate is `mantissa * 5**1126` for the
      !! smallest subnormal double, which needs 803 decimal digits, so 100
      !! limbs (900 digits) is comfortably sufficient.
   integer(default_int), parameter :: MAX_DIGITS = MAX_LIMBS*LIMB_DIGITS
      !! Decimal digit capacity of the scratch big integer.
   integer(default_int), parameter :: WORK_LEN = MAX_DIGITS + PIC_FORMAT_MAX_DECIMALS + 2_default_int
      !! Length of the digit string scratch buffers.

   character(len=*), parameter :: ALL_DIGITS = "0123456789"
      !! Digit lookup table, mirroring the table driven integer conversion in
      !! `pic_strings_to_strings`. Position `d + 1` holds the character for the
      !! value `d`, so the position of a digit is also the character of its
      !! successor, which is what the carry propagation needs.
   character(len=*), parameter :: ODD_DIGITS = "13579"
      !! Used to test the parity of the last retained digit for half to even.

   !> Render a real in fixed point notation with an exact number of decimals.
   interface to_string_fixed
      !! Fixed point rendering, never an exponent.
      !!
      !! The result carries exactly `decimals` digits after the decimal point,
      !! correctly rounded round half to even from the exact binary value. A
      !! leading `0` is always written for magnitudes below one, and `decimals`
      !! equal to zero produces no trailing decimal point. `decimals` is clamped
      !! to the range `[0, PIC_FORMAT_MAX_DECIMALS]`.
      module procedure :: to_string_fixed_sp
      module procedure :: to_string_fixed_dp
   end interface to_string_fixed

   !> Render a real in scientific notation with a canonical exponent field.
   interface to_string_sci
      !! Scientific rendering, canonical form `[-]D[.DDD]e[+-]NN`.
      !!
      !! Exactly `sig_digits` significant digits are produced, one before the
      !! decimal point and the rest after it; `sig_digits` equal to one yields
      !! no decimal point. The exponent letter is always a lowercase `e`, the
      !! exponent sign is always present, and the exponent magnitude is written
      !! with a minimum of two digits, zero padded on the left (three digits are
      !! used when the magnitude requires them, as for `1.0e-300`). Zero is
      !! rendered with an exponent of zero. `sig_digits` is clamped to the range
      !! `[1, PIC_FORMAT_MAX_SIG_DIGITS]`.
      module procedure :: to_string_sci_sp
      module procedure :: to_string_sci_dp
   end interface to_string_sci

   !> Render an integer right aligned in a field of the requested width.
   interface to_string_width
      !! Right aligned integer rendering, blank padded on the left.
      !!
      !! The value is never truncated and never replaced by asterisks: if it
      !! does not fit in `width` characters the field is widened and the full
      !! number is returned, so the result length is `max(width, needed)`. A
      !! `width` of zero or less simply returns the unpadded number.
      module procedure :: to_string_width_i32
      module procedure :: to_string_width_i64
   end interface to_string_width

contains

   pure function to_string_fixed_sp(value, decimals) result(string)
      !! Fixed point rendering of a `real(sp)` value.
      !!
      !! Widening to `real(dp)` is exact for every finite `real(sp)` value and
      !! preserves NaN and both infinities, so the double precision expansion
      !! produces the same digits.
      real(sp), intent(in) :: value
      integer(default_int), intent(in) :: decimals
      character(len=:), allocatable :: string

      string = to_string_fixed_dp(real(value, dp), decimals)

   end function to_string_fixed_sp

   pure function to_string_fixed_dp(value, decimals) result(string)
      !! Fixed point rendering of a `real(dp)` value.
      real(dp), intent(in) :: value
      integer(default_int), intent(in) :: decimals
      character(len=:), allocatable :: string

      character(len=WORK_LEN) :: exact, rounded
      integer(default_int) :: n_exact, point_pos, dec, n_keep, n_rounded, bump
      logical :: negative, special

      call special_text(value, string, special)
      if (special) return

      dec = max(0_default_int, min(decimals, PIC_FORMAT_MAX_DECIMALS))
      negative = is_negative(value)

      if (value == 0.0_dp) then
         ! Zero never carries a sign. Recognising a negative zero requires SIGN
         ! to distinguish the two signed zeros, which Fortran leaves processor
         ! dependent (F2018 16.9.165: the result is |A| if the processor cannot
         ! distinguish them), and Intel and AOCC do not distinguish them under
         ! their default fast floating point model. Emitting "-0.00" would
         ! therefore be compiler dependent, which is the one thing this module
         ! exists to avoid. A non-zero value that merely rounds to zero does
         ! keep its sign: SIGN is well defined for a non-zero argument.
         negative = .false.
         rounded = ""
         n_rounded = 0_default_int
         point_pos = 0_default_int
      else
         call expand_exact(abs(value), exact, n_exact, point_pos)
         n_keep = point_pos + dec
         call round_decimal(exact, n_exact, n_keep, rounded, n_rounded, bump)
         point_pos = point_pos + bump
      end if

      string = assemble_fixed(negative, rounded, n_rounded, point_pos, dec)

   end function to_string_fixed_dp

   pure function to_string_sci_sp(value, sig_digits) result(string)
      !! Scientific rendering of a `real(sp)` value. See `to_string_fixed_sp`
      !! for why the widening to `real(dp)` is lossless.
      real(sp), intent(in) :: value
      integer(default_int), intent(in) :: sig_digits
      character(len=:), allocatable :: string

      string = to_string_sci_dp(real(value, dp), sig_digits)

   end function to_string_sci_sp

   pure function to_string_sci_dp(value, sig_digits) result(string)
      !! Scientific rendering of a `real(dp)` value.
      real(dp), intent(in) :: value
      integer(default_int), intent(in) :: sig_digits
      character(len=:), allocatable :: string

      character(len=WORK_LEN) :: exact, rounded
      integer(default_int) :: n_exact, point_pos, sig, n_rounded, bump, exp10
      logical :: negative, special

      call special_text(value, string, special)
      if (special) return

      sig = max(1_default_int, min(sig_digits, PIC_FORMAT_MAX_SIG_DIGITS))
      negative = is_negative(value)

      if (value == 0.0_dp) then
         ! Zero never carries a sign. Recognising a negative zero requires SIGN
         ! to distinguish the two signed zeros, which Fortran leaves processor
         ! dependent (F2018 16.9.165: the result is |A| if the processor cannot
         ! distinguish them), and Intel and AOCC do not distinguish them under
         ! their default fast floating point model. Emitting "-0.00" would
         ! therefore be compiler dependent, which is the one thing this module
         ! exists to avoid. A non-zero value that merely rounds to zero does
         ! keep its sign: SIGN is well defined for a non-zero argument.
         negative = .false.
         rounded = repeat("0", sig)
         exp10 = 0_default_int
      else
         call expand_exact(abs(value), exact, n_exact, point_pos)
         call round_decimal(exact, n_exact, sig, rounded, n_rounded, bump)
         ! A carry out of the leading digit turns "999" into "1000"; only the
         ! first sig digits are used, so the extra trailing zero is ignored and
         ! the decimal exponent moves up by one instead.
         exp10 = point_pos + bump - 1_default_int
      end if

      if (sig > 1_default_int) then
         string = rounded(1:1)//"."//rounded(2:sig)
      else
         string = rounded(1:1)
      end if
      string = string//"e"//exponent_field(exp10)
      if (negative) string = "-"//string

   end function to_string_sci_dp

   pure function to_string_width_i32(value, width) result(string)
      !! Right aligned rendering of an `integer(int32)` value.
      integer(int32), intent(in) :: value
      integer(default_int), intent(in) :: width
      character(len=:), allocatable :: string

      string = pad_left(to_string(value), width)

   end function to_string_width_i32

   pure function to_string_width_i64(value, width) result(string)
      !! Right aligned rendering of an `integer(int64)` value.
      integer(int64), intent(in) :: value
      integer(default_int), intent(in) :: width
      character(len=:), allocatable :: string

      string = pad_left(to_string(value), width)

   end function to_string_width_i64

   pure function pad_left(text, width) result(string)
      !! Blank pad on the left, widening rather than truncating.
      character(len=*), intent(in) :: text
      integer(default_int), intent(in) :: width
      character(len=:), allocatable :: string

      if (width > len(text)) then
         string = repeat(" ", width - len(text))//text
      else
         string = text
      end if

   end function pad_left

   pure subroutine special_text(value, string, special)
      !! Detect NaN and the infinities and return their canonical spelling.
      !!
      !! `ieee_arithmetic` is deliberately not used: its availability is uneven
      !! across the compilers PIC targets. `value /= value` is plain standard
      !! Fortran and, unlike `<` or `>`, is a quiet IEEE comparison, so it does
      !! not raise INVALID under the `-ffpe-trap=invalid` debug build. NaN is
      !! therefore tested before the ordered comparisons against `huge`.
      !!
      !! This is a subroutine rather than a function because a pure function may
      !! not have an `intent(out)` dummy argument.
      real(dp), intent(in) :: value
      character(len=:), allocatable, intent(out) :: string
      logical, intent(out) :: special

      special = .true.
      if (value /= value) then
         string = PIC_FORMAT_NAN
      else if (value > huge(value)) then
         string = PIC_FORMAT_INF
      else if (value < -huge(value)) then
         string = PIC_FORMAT_NEG_INF
      else
         special = .false.
         string = ""
      end if

   end subroutine special_text

   pure function is_negative(value) result(negative)
      !! True for negative values. The result for negative zero is processor
      !! dependent, so callers must not rely on it: every caller here tests
      !! `value == 0` first and drops the sign for zero. Not valid for NaN,
      !! which the callers have already filtered out.
      real(dp), intent(in) :: value
      logical :: negative

      negative = sign(1.0_dp, value) < 0.0_dp

   end function is_negative

   pure function exponent_field(exp10) result(string)
      !! Canonical exponent field: sign, then at least two zero padded digits.
      integer(default_int), intent(in) :: exp10
      character(len=:), allocatable :: string
      character(len=:), allocatable :: magnitude

      magnitude = to_string(abs(exp10))
      if (len(magnitude) < 2) magnitude = "0"//magnitude
      if (exp10 < 0_default_int) then
         string = "-"//magnitude
      else
         string = "+"//magnitude
      end if

   end function exponent_field

   pure function assemble_fixed(negative, rounded, n_rounded, point_pos, dec) result(string)
      !! Place the decimal point in an already rounded digit string.
      !!
      !! The invariant maintained by `round_decimal` is that `n_rounded` equals
      !! `point_pos + dec` whenever any digit survives, so splitting at
      !! `point_pos` always leaves exactly `dec` fractional digits.
      logical, intent(in) :: negative
      character(len=*), intent(in) :: rounded
      integer(default_int), intent(in) :: n_rounded, point_pos, dec
      character(len=:), allocatable :: string
      character(len=:), allocatable :: int_part, frac_part

      if (n_rounded <= 0_default_int) then
         int_part = "0"
         frac_part = repeat("0", dec)
      else if (point_pos <= 0_default_int) then
         int_part = "0"
         frac_part = repeat("0", -point_pos)//rounded(1:n_rounded)
      else
         int_part = rounded(1:point_pos)
         frac_part = rounded(point_pos + 1:n_rounded)
      end if

      if (dec > 0_default_int) then
         string = int_part//"."//frac_part
      else
         string = int_part
      end if
      if (negative) string = "-"//string

   end function assemble_fixed

   pure subroutine expand_exact(value, exact, n_exact, point_pos)
      !! Exact decimal expansion of a finite, strictly positive value.
      !!
      !! An IEEE value is `mantissa * 2**pow2` with an integer mantissa. When
      !! `pow2` is negative that equals `mantissa * 5**(-pow2) / 10**(-pow2)`,
      !! so the exact decimal digits are the digits of the big integer
      !! `mantissa * 5**(-pow2)` with the point shifted `-pow2` places. When
      !! `pow2` is positive the value is the integer `mantissa * 2**pow2`.
      !! Either way no rounding whatsoever happens here.
      !!
      !! On return the value equals `0.<digits> * 10**point_pos`, where
      !! `<digits>` is `exact(1:n_exact)`.
      real(dp), intent(in) :: value
      character(len=*), intent(out) :: exact
      integer(default_int), intent(out) :: n_exact
      integer(default_int), intent(out) :: point_pos

      integer(int64) :: limb(MAX_LIMBS)
      integer(int64) :: mantissa
      integer(default_int) :: n_limb, n_bits, pow2, shift

      n_bits = int(digits(value), default_int)
      pow2 = int(exponent(value), default_int) - n_bits
      mantissa = int(scale(fraction(value), n_bits), int64)

      call bd_set(limb, n_limb, mantissa)
      shift = 0_default_int
      if (pow2 > 0_default_int) then
         call bd_mul_pow(limb, n_limb, 2_int64, pow2)
      else if (pow2 < 0_default_int) then
         call bd_mul_pow(limb, n_limb, 5_int64, -pow2)
         shift = -pow2
      end if

      call bd_to_digits(limb, n_limb, exact, n_exact)
      point_pos = n_exact - shift

   end subroutine expand_exact

   pure subroutine round_decimal(exact, n_exact, n_keep, rounded, n_rounded, bump)
      !! Round an exact digit string to `n_keep` leading digits, half to even.
      !!
      !! `bump` is one when the rounding carried out of the leading digit (for
      !! example `999` becoming `1000`), in which case the caller must advance
      !! the decimal point by one.
      character(len=*), intent(in) :: exact
      integer(default_int), intent(in) :: n_exact, n_keep
      character(len=*), intent(out) :: rounded
      integer(default_int), intent(out) :: n_rounded, bump
      integer(default_int) :: i
      logical :: round_up, rest_nonzero, last_odd
      character(len=1) :: next_digit

      rounded = ""
      bump = 0_default_int

      if (n_keep < 0_default_int) then
         ! Every digit lies below the rounding position, so the value is zero
         ! to the requested precision and cannot round up.
         n_rounded = 0_default_int
         return
      end if

      if (n_keep >= n_exact) then
         ! Nothing to discard; pad with the zeros that follow the expansion.
         rounded(1:n_exact) = exact(1:n_exact)
         do i = n_exact + 1_default_int, n_keep
            rounded(i:i) = "0"
         end do
         n_rounded = n_keep
         return
      end if

      if (n_keep > 0_default_int) rounded(1:n_keep) = exact(1:n_keep)
      n_rounded = n_keep

      next_digit = exact(n_keep + 1:n_keep + 1)
      rest_nonzero = .false.
      do i = n_keep + 2_default_int, n_exact
         if (exact(i:i) /= "0") then
            rest_nonzero = .true.
            exit
         end if
      end do

      if (next_digit > "5") then
         round_up = .true.
      else if (next_digit < "5") then
         round_up = .false.
      else if (rest_nonzero) then
         round_up = .true.
      else
         ! Exact tie: round to even. With no retained digit the implied value
         ! is zero, which is even.
         if (n_keep == 0_default_int) then
            last_odd = .false.
         else
            last_odd = index(ODD_DIGITS, rounded(n_keep:n_keep)) > 0
         end if
         round_up = last_odd
      end if

      if (round_up) call increment_digits(rounded, n_rounded, bump)

   end subroutine round_decimal

   pure subroutine increment_digits(rounded, n_rounded, bump)
      !! Add one to a decimal digit string, growing it on carry out.
      character(len=*), intent(inout) :: rounded
      integer(default_int), intent(inout) :: n_rounded
      integer(default_int), intent(out) :: bump
      integer(default_int) :: i, successor
      logical :: carry

      bump = 0_default_int
      carry = .true.
      i = n_rounded
      do while (carry .and. i >= 1_default_int)
         if (rounded(i:i) == "9") then
            rounded(i:i) = "0"
         else
            successor = index(ALL_DIGITS, rounded(i:i)) + 1_default_int
            rounded(i:i) = ALL_DIGITS(successor:successor)
            carry = .false.
         end if
         i = i - 1_default_int
      end do

      if (carry) then
         ! Every retained digit was a nine and is now a zero, so the result is
         ! a leading one followed by that many zeros. Order matters when
         ! `n_rounded` is zero: both assignments then address position one.
         rounded(n_rounded + 1:n_rounded + 1) = "0"
         rounded(1:1) = "1"
         n_rounded = n_rounded + 1_default_int
         bump = 1_default_int
      end if

   end subroutine increment_digits

   pure subroutine bd_set(limb, n_limb, value)
      !! Initialise the big integer from a strictly positive `int64`.
      integer(int64), intent(out) :: limb(:)
      integer(default_int), intent(out) :: n_limb
      integer(int64), intent(in) :: value
      integer(int64) :: remaining

      limb = 0_int64
      remaining = value
      n_limb = 0_default_int
      do while (remaining > 0_int64)
         n_limb = n_limb + 1_default_int
         limb(n_limb) = mod(remaining, LIMB_BASE)
         remaining = remaining/LIMB_BASE
      end do

   end subroutine bd_set

   pure subroutine bd_mul_small(limb, n_limb, factor)
      !! Multiply the big integer in place by a factor smaller than `LIMB_BASE`.
      !!
      !! The widest product is `(LIMB_BASE-1)*factor + carry`, below 1.0e18,
      !! which fits an `int64` with room to spare. `int64` is used deliberately:
      !! the arithmetic must not narrow in a default int32 build.
      integer(int64), intent(inout) :: limb(:)
      integer(default_int), intent(inout) :: n_limb
      integer(int64), intent(in) :: factor
      integer(default_int) :: i
      integer(int64) :: carry, product

      carry = 0_int64
      do i = 1_default_int, n_limb
         product = limb(i)*factor + carry
         limb(i) = mod(product, LIMB_BASE)
         carry = product/LIMB_BASE
      end do
      do while (carry > 0_int64)
         n_limb = n_limb + 1_default_int
         limb(n_limb) = mod(carry, LIMB_BASE)
         carry = carry/LIMB_BASE
      end do

   end subroutine bd_mul_small

   pure subroutine bd_mul_pow(limb, n_limb, base, power)
      !! Multiply the big integer in place by `base**power`.
      !!
      !! The exponent is consumed in the largest chunks whose power still fits
      !! below `LIMB_BASE`, which is 29 for base two and 12 for base five.
      integer(int64), intent(inout) :: limb(:)
      integer(default_int), intent(inout) :: n_limb
      integer(int64), intent(in) :: base
      integer(default_int), intent(in) :: power
      integer(default_int) :: chunk, remaining
      integer(int64) :: step, factor

      chunk = 0_default_int
      step = 1_int64
      do while (step*base < LIMB_BASE)
         step = step*base
         chunk = chunk + 1_default_int
      end do

      remaining = power
      do while (remaining > 0_default_int)
         if (remaining >= chunk) then
            factor = step
            remaining = remaining - chunk
         else
            factor = base**remaining
            remaining = 0_default_int
         end if
         call bd_mul_small(limb, n_limb, factor)
      end do

   end subroutine bd_mul_pow

   pure subroutine bd_to_digits(limb, n_limb, exact, n_exact)
      !! Write the big integer out as a decimal digit string, no leading zeros.
      integer(int64), intent(in) :: limb(:)
      integer(default_int), intent(in) :: n_limb
      character(len=*), intent(out) :: exact
      integer(default_int), intent(out) :: n_exact
      character(len=LIMB_DIGITS) :: chunk
      integer(default_int) :: i, first, pos

      exact = ""
      call limb_to_digits(limb(n_limb), chunk)
      first = 1_default_int
      do while (first < LIMB_DIGITS .and. chunk(first:first) == "0")
         first = first + 1_default_int
      end do
      pos = LIMB_DIGITS - first + 1_default_int
      exact(1:pos) = chunk(first:LIMB_DIGITS)

      do i = n_limb - 1_default_int, 1_default_int, -1_default_int
         call limb_to_digits(limb(i), chunk)
         exact(pos + 1_default_int:pos + LIMB_DIGITS) = chunk
         pos = pos + LIMB_DIGITS
      end do

      n_exact = pos

   end subroutine bd_to_digits

   pure subroutine limb_to_digits(value, chunk)
      !! Expand one limb into exactly `LIMB_DIGITS` zero padded characters,
      !! table driven the same way `pic_strings` converts integers.
      integer(int64), intent(in) :: value
      character(len=*), intent(out) :: chunk
      integer(default_int) :: i, digit
      integer(int64) :: remaining

      remaining = value
      do i = LIMB_DIGITS, 1_default_int, -1_default_int
         digit = int(mod(remaining, 10_int64), default_int) + 1_default_int
         chunk(i:i) = ALL_DIGITS(digit:digit)
         remaining = remaining/10_int64
      end do

   end subroutine limb_to_digits

end module pic_format
