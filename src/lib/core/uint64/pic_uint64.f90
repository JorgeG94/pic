! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Arithmetic modulo 2**64 on `integer(int64)`, without signed overflow.
module pic_uint64
   !! Unsigned 64-bit arithmetic for algorithms that are specified in terms of
   !! wrapping 64-bit words: hashes, counter-based generators, fixed-point
   !! tables.
   !!
   !! ### Why these exist
   !!
   !! Fortran has no unsigned integer type, and signed integer overflow is not
   !! defined by the standard. The obvious spelling of a wrapping multiply is
   !! therefore undefined behaviour, and a build with `-ftrapv` or
   !! `-fsanitize=signed-integer-overflow` may abort on it. Every routine here
   !! splits its operands into limbs so that no intermediate ever leaves the
   !! range a signed 64-bit integer can represent.
   !!
   !! Bit manipulation goes through `ibits`, which is defined on the integer
   !! bit model and is therefore correct for negative operands: a negative
   !! `integer(int64)` widened through `ibits` keeps its two's-complement
   !! pattern, which is exactly the unsigned value being modelled. `ishft` is
   !! used only with non-negative (left-shift) counts.
   !!
   !! ### Interpreting the values
   !!
   !! A 64-bit unsigned value is carried in an `integer(int64)` holding its
   !! two's-complement bit pattern, so values at or above 2**63 appear as
   !! negative Fortran integers. Fortran's own `<` therefore compares them as
   !! signed; use `u64_less` when the unsigned ordering is wanted.
   use pic_types, only: default_int, int64

   implicit none
   private

   public :: u64_add
   public :: u64_mul
   public :: u64_shr
   public :: u64_less

   integer(int64), parameter :: MASK16 = 65535_int64
      !! Low 16-bit mask, used by the limb arithmetic below.
   integer(int64), parameter :: MASK32 = 4294967295_int64
      !! Low 32-bit mask, used by the limb arithmetic below.

contains

   pure function u64_shr(x, n) result(r)
      !! Logical (zero-filling) right shift of a 64-bit word by `n` bits,
      !! `0 <= n <= 63`. Written with `ibits` rather than `ishft(x, -n)` or the
      !! Fortran 2008 `shiftr` for the portability reasons given in the module
      !! documentation.
      integer(int64), intent(in) :: x
      integer(default_int), intent(in) :: n
      integer(int64) :: r

      ! A shift of zero is handled separately rather than falling through to
      ! `ibits(x, 0, 64)`. That call is conforming -- F2018 16.9.105 requires
      ! only POS + LEN <= BIT_SIZE(I) -- but LFortran 0.65.0 returns 0 from it
      ! for every input, while `ibits(x, 1, 63)` is correct. Reproducer in
      ! test/test_pic_uint64.f90 ("shr by 0 is identity").
      if (n <= 0_default_int) then
         r = x
      else
         r = ibits(x, n, 64 - n)
      end if
   end function u64_shr

   pure function u64_add(a, b) result(r)
      !! `(a + b)` reduced modulo 2**64, without ever overflowing a signed
      !! 64-bit integer. Operands are split into 32-bit halves with `ibits`, so
      !! each half sum stays below 2**33.
      integer(int64), intent(in) :: a
      integer(int64), intent(in) :: b
      integer(int64) :: r

      integer(int64) :: low, high

      low = ibits(a, 0, 32) + ibits(b, 0, 32)
      high = ibits(a, 32, 32) + ibits(b, 32, 32) + ibits(low, 32, 32)
      r = ior(ishft(iand(high, MASK32), 32), iand(low, MASK32))
   end function u64_add

   pure function u64_mul(a, b) result(r)
      !! `(a * b)` reduced modulo 2**64, without ever overflowing a signed
      !! 64-bit integer.
      !!
      !! Schoolbook multiplication on 16-bit limbs: each partial product is
      !! below 2**32 and at most four of them plus a carry are accumulated, so
      !! every intermediate stays below 2**35 and signed overflow is impossible
      !! regardless of the sign of the inputs. Limbs are extracted with `ibits`
      !! (bit-model, sign-safe) and the result is reassembled with `ior`/`ishft`.
      integer(int64), intent(in) :: a
      integer(int64), intent(in) :: b
      integer(int64) :: r

      integer(int64) :: a0, a1, a2, a3
      integer(int64) :: b0, b1, b2, b3
      integer(int64) :: t, carry
      integer(int64) :: r0, r1, r2, r3

      a0 = ibits(a, 0, 16)
      a1 = ibits(a, 16, 16)
      a2 = ibits(a, 32, 16)
      a3 = ibits(a, 48, 16)
      b0 = ibits(b, 0, 16)
      b1 = ibits(b, 16, 16)
      b2 = ibits(b, 32, 16)
      b3 = ibits(b, 48, 16)

      t = a0*b0
      r0 = iand(t, MASK16)
      carry = ibits(t, 16, 48)

      t = a0*b1 + a1*b0 + carry
      r1 = iand(t, MASK16)
      carry = ibits(t, 16, 48)

      t = a0*b2 + a1*b1 + a2*b0 + carry
      r2 = iand(t, MASK16)
      carry = ibits(t, 16, 48)

      t = a0*b3 + a1*b2 + a2*b1 + a3*b0 + carry
      r3 = iand(t, MASK16)

      r = ior(ior(ishft(r3, 48), ishft(r2, 32)), ior(ishft(r1, 16), r0))
   end function u64_mul

   pure function u64_less(a, b) result(r)
      !! `.true.` when `a < b` with both operands read as unsigned 64-bit
      !! values.
      !!
      !! Fortran's `<` compares the two's-complement patterns as signed, so it
      !! reports `huge(0_int64) + 1` (which is 2**63 unsigned) as *less than*
      !! zero. Flipping the sign bit of both operands maps the unsigned order
      !! onto the signed order exactly, which is why this is a comparison of
      !! `ieor(a, SIGN_BIT)` against `ieor(b, SIGN_BIT)` and not a sequence of
      !! sign tests.
      integer(int64), intent(in) :: a
      integer(int64), intent(in) :: b
      logical :: r

      integer(int64), parameter :: SIGN_BIT = ishft(1_int64, 63)

      r = ieor(a, SIGN_BIT) < ieor(b, SIGN_BIT)
   end function u64_less

end module pic_uint64
