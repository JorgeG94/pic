! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Reproducible pseudo-random number generation with explicit state.
!!
!! The Fortran intrinsic `random_number` is deliberately unspecified: every
!! compiler is free to ship a different engine, a different seeding rule and a
!! different number of seed words. A code whose results depend on it therefore
!! cannot be regression tested across GNU, Intel, NVIDIA HPC, LFortran, Cray or
!! Flang. `pic_rng` provides explicit-state counter/LCG generators whose output
!! is a pure function of the state, so the same seed yields the same bits on
!! every compiler and every platform.
module pic_rng
   !! Portable, reproducible pseudo-random number generators.
   !!
   !! Two engines are provided:
   !!
   !! * `splitmix64_t` - SplitMix64 (Steele, Lea and Flood, 2014; reference
   !!   implementation by Sebastiano Vigna, public domain). A 64-bit counter
   !!   plus a strong finalising mix. Tiny, fast, and ideal for seeding other
   !!   generators.
   !! * `pcg32_t` - PCG-XSH-RR 64/32 (O'Neill, 2014). A 64-bit LCG whose output
   !!   is a permuted 32-bit word. Supports 2**63 distinct streams through its
   !!   increment.
   !!
   !! Both types expose `seed`, `next` and `next_u64`, and both work with the
   !! module-level generics `next_real_dp`, `next_below` and `stream_for`.
   !!
   !! ### Portability notes (the whole reason this module exists)
   !!
   !! 1. **No BOZ literals anywhere.** The 64-bit constants of SplitMix64 have
   !!    their top bit set, so `z'9E3779B97F4A7C15'` is not representable as a
   !!    positive `integer(int64)`. Compilers disagree on BOZ literals appearing
   !!    in initialisation expressions (gfortran, ifx and nvfortran all differ in
   !!    what they accept and what they warn about). Every constant below is
   !!    therefore written as an ordinary signed decimal literal equal to the
   !!    two's-complement interpretation of the intended bit pattern, with the
   !!    hexadecimal value given in a comment.
   !! 2. **No reliance on signed overflow.** Fortran has no unsigned integer
   !!    type, and signed overflow is not defined by the standard: a build with
   !!    `-ftrapv` or `-fsanitize=signed-integer-overflow` may abort on it. All
   !!    modular arithmetic goes through `u64_add` and `u64_mul`, which split
   !!    their operands into 16-bit limbs with `ibits` so that every intermediate
   !!    product stays below 2**35 and every accumulation stays well inside the
   !!    signed 64-bit range. The results are reassembled with `ior`/`ishft`,
   !!    which are bit-model intrinsics and are exactly defined for every bit
   !!    pattern, sign bit included.
   !! 3. **No `transfer` between reals and integers.** nvfortran and LFortran do
   !!    not agree on the result of bit-casting a double to an integer. Reals are
   !!    built arithmetically instead (see `next_real_dp`).
   !! 4. **No right shift by a negative count and no F2008 shift intrinsics.**
   !!    `ishft(x, -n)` is standard but its interaction with the sign bit is a
   !!    recurring source of confusion, and `shiftr`/`shifta` are Fortran 2008
   !!    and not uniformly available on the older compilers PIC targets. Every
   !!    logical right shift here is written as `ibits(x, n, 64 - n)`, which is
   !!    Fortran 90, zero-filling by definition, and correct for negative `x`.
   !!    `ishft` is used only with non-negative (left-shift) counts.
   use pic_types, only: default_int, dp, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_uint64, only: u64_add, u64_mul, u64_shr
   implicit none
   private

   public :: splitmix64_t
   public :: pcg32_t
   public :: next_real_dp
   public :: next_below
   public :: stream_for

   integer(int64), parameter :: MASK32 = 4294967295_int64
      !! Low 32-bit mask (2**32 - 1).
   integer(int64), parameter :: TWO_TO_31 = 2147483648_int64
      !! 2**31, the first value that does not fit in a signed 32-bit integer.
   integer(int64), parameter :: TWO_TO_32 = 4294967296_int64
      !! 2**32, used to fold an unsigned 32-bit value into `int32`.

   integer(int64), parameter :: GOLDEN_GAMMA = -7046029254386353131_int64
      !! SplitMix64 additive constant, bit pattern 0x9E3779B97F4A7C15.
   integer(int64), parameter :: MIX_C1 = -4658895280553007687_int64
      !! SplitMix64 first multiplier, bit pattern 0xBF58476D1CE4E5B9.
   integer(int64), parameter :: MIX_C2 = -7723592293110705685_int64
      !! SplitMix64 second multiplier, bit pattern 0x94D049BB133111EB.

   integer(int64), parameter :: PCG_MULTIPLIER = 6364136223846793005_int64
      !! PCG 64-bit LCG multiplier, bit pattern 0x5851F42D4C957F2D.
   integer(int64), parameter :: PCG_DEFAULT_INCREMENT = 1442695040888963407_int64
      !! Default PCG stream increment, bit pattern 0x14057B7EF767814F.

   real(dp), parameter :: TWO_TO_MINUS_53 = 1.0_dp/9007199254740992.0_dp
      !! 2**-53, exactly representable in IEEE binary64.

   type :: splitmix64_t
      !! SplitMix64 generator: a 64-bit counter run through a strong finaliser.
      integer(int64) :: state = 0_int64
         !! Explicit 64-bit state. Fixed width on purpose: the algorithm is
         !! defined on 64-bit words and must not follow `default_int`.
   contains
      procedure :: seed => splitmix64_seed
      procedure :: next => splitmix64_next
      procedure :: next_u64 => splitmix64_next_u64
   end type splitmix64_t

   type :: pcg32_t
      !! PCG-XSH-RR 64/32 generator: 64-bit LCG state with a 32-bit output
      !! permutation, plus a per-stream odd increment.
      integer(int64) :: state = 0_int64
         !! Explicit 64-bit LCG state (fixed width, not `default_int`).
      integer(int64) :: increment = PCG_DEFAULT_INCREMENT
         !! Stream increment; always odd, which is what makes streams distinct.
   contains
      procedure :: seed => pcg32_seed
      procedure :: next => pcg32_next
      procedure :: next_u64 => pcg32_next_u64
   end type pcg32_t

   interface next_real_dp
      !! Draw a uniform `real(dp)` in [0, 1).
      !!
      !! Construction, identical for both engines and on every compiler: take a
      !! fresh 64-bit word `w` from the generator, keep its top 53 bits
      !! `u = ibits(w, 11, 53)` (an integer in [0, 2**53)), and return
      !! `real(u, dp) * 2**-53`. Every integer below 2**53 is exact in IEEE
      !! binary64 and 2**-53 is a power of two, so the product is exact: no
      !! rounding, no compiler-dependent fused multiply-add, and the result can
      !! never reach 1.0. Values are therefore multiples of 2**-53 in
      !! [0, 1 - 2**-53].
      module procedure splitmix64_next_real_dp
      module procedure pcg32_next_real_dp
   end interface next_real_dp

   interface next_below
      !! Draw a uniform integer in [0, n) by rejection sampling.
      !!
      !! The naive `mod(next(), n)` is biased whenever `n` does not divide the
      !! generator's period: the first `2**64 mod n` residues each get one extra
      !! preimage, so for large `n` some outcomes are measurably more likely.
      !! Instead a mask of the smallest `2**k - 1` at least `n - 1` is applied to
      !! a fresh 64-bit word and values at or above `n` are rejected and redrawn.
      !! Every accepted value has exactly one preimage per draw, so the result is
      !! exactly uniform. The acceptance probability is above 1/2, so the
      !! expected number of draws is below 2 and the loop terminates with
      !! probability one.
      !!
      !! `n == 1` returns 0 without consuming a draw. `n <= 0` is a usage error:
      !! it returns 0 and, if `error` is present, sets `ERROR_VALIDATION`.
      module procedure splitmix64_next_below
      module procedure pcg32_next_below
   end interface next_below

   interface stream_for
      !! Derive an independent, reproducible generator for a stream id.
      !!
      !! Parallel codes must not let the seed of a task depend on scheduling
      !! order. `stream_for` maps `(master_seed, stream_id)` to a generator
      !! through two SplitMix64 finalisers, so stream 7 is the same sequence
      !! whether it runs first, last or on another rank, and different ids give
      !! uncorrelated sequences.
      module procedure stream_for_splitmix64
      module procedure stream_for_pcg32
   end interface stream_for

contains

   pure function mix64(x) result(r)
      !! The SplitMix64 finalising mix (the "MurmurHash3-style" avalanche used
      !! by `splitmix64.c`). Used both as the generator output stage and as the
      !! seed derivation function for `stream_for`.
      integer(int64), intent(in) :: x
      integer(int64) :: r

      r = u64_mul(ieor(x, u64_shr(x, 30_default_int)), MIX_C1)
      r = u64_mul(ieor(r, u64_shr(r, 27_default_int)), MIX_C2)
      r = ieor(r, u64_shr(r, 31_default_int))
   end function mix64

   pure function rotate_right32(x, rot) result(r)
      !! Rotate the low 32 bits of `x` right by `rot` bits, `0 <= rot <= 31`.
      !! Branch free: when `rot` is zero the left-shifted term is shifted clean
      !! out of the low 32 bits and the mask removes it.
      integer(int64), intent(in) :: x
      integer(int64), intent(in) :: rot
      integer(int64) :: r

      r = ior(ibits(x, int(rot, default_int), int(32 - rot, default_int)), &
              iand(ishft(iand(x, MASK32), int(32 - rot, default_int)), MASK32))
   end function rotate_right32

   pure function u32_to_int32(u) result(r)
      !! Reinterpret an unsigned 32-bit value held in [0, 2**32) as the signed
      !! `int32` with the same bit pattern. Done arithmetically rather than with
      !! `transfer`, and without ever passing an out-of-range value to `int`.
      integer(int64), intent(in) :: u
      integer(int32) :: r

      if (u >= TWO_TO_31) then
         r = int(u - TWO_TO_32, int32)
      else
         r = int(u, int32)
      end if
   end function u32_to_int32

   pure function rejection_mask(n) result(mask)
      !! Smallest `2**k - 1` that is at least `n - 1`, for `n >= 1`.
      integer(int64), intent(in) :: n
      integer(int64) :: mask

      mask = 0_int64
      do while (mask < n - 1_int64)
         mask = ior(ishft(mask, 1), 1_int64)
      end do
   end function rejection_mask

   pure function real_from_word(word) result(r)
      !! Turn a 64-bit random word into a `real(dp)` in [0, 1) using its top 53
      !! bits. See the `next_real_dp` interface documentation for the exactness
      !! argument.
      integer(int64), intent(in) :: word
      real(dp) :: r

      r = real(ibits(word, 11, 53), dp)*TWO_TO_MINUS_53
   end function real_from_word

   pure function stream_seed(master_seed, stream_id) result(s)
      !! Deterministic seed for `stream_id` under `master_seed`.
      !!
      !! `mix64` is applied to the master seed, the stream id is spread over the
      !! whole word by multiplying `stream_id + 1` by the golden-ratio constant,
      !! the two are combined with `ieor`, and the result is mixed once more.
      !! Adding one to the id keeps stream 0 from collapsing the multiplication
      !! to zero. Only bit-model and modular arithmetic is used, so the mapping
      !! is identical on every compiler.
      integer(int64), intent(in) :: master_seed
      integer(default_int), intent(in) :: stream_id
      integer(int64) :: s

      s = mix64(ieor(mix64(master_seed), &
                     u64_mul(u64_add(int(stream_id, int64), 1_int64), GOLDEN_GAMMA)))
   end function stream_seed

   subroutine splitmix64_seed(self, s)
      !! Set the generator state. Any 64-bit value is a valid seed, including
      !! zero: SplitMix64 has no degenerate state.
      class(splitmix64_t), intent(inout) :: self
      integer(int64), intent(in) :: s

      self%state = s
   end subroutine splitmix64_seed

   function splitmix64_next(self) result(r)
      !! Advance the state by the golden-ratio increment and return the mixed
      !! 64-bit output.
      class(splitmix64_t), intent(inout) :: self
      integer(int64) :: r

      self%state = u64_add(self%state, GOLDEN_GAMMA)
      r = mix64(self%state)
   end function splitmix64_next

   function splitmix64_next_u64(self) result(r)
      !! Full 64-bit random word. For SplitMix64 this is simply `next`; it
      !! exists so that both engines present the same interface to the shared
      !! real and bounded-integer conversions.
      class(splitmix64_t), intent(inout) :: self
      integer(int64) :: r

      r = self%next()
   end function splitmix64_next_u64

   subroutine pcg32_seed(self, state, seq)
      !! Seed the generator, following the reference `pcg32_srandom_r`: the
      !! stream selector is shifted left and forced odd to become the LCG
      !! increment, then the state is warmed up, offset by the requested state
      !! and warmed up again.
      class(pcg32_t), intent(inout) :: self
      integer(int64), intent(in) :: state
      integer(int64), intent(in) :: seq

      integer(int64) :: discard

      self%increment = ior(ishft(seq, 1), 1_int64)
      self%state = 0_int64
      discard = pcg32_step(self)
      self%state = u64_add(self%state, state)
      discard = pcg32_step(self)
   end subroutine pcg32_seed

   function pcg32_step(self) result(r)
      !! Advance the LCG one step and return the XSH-RR permutation of the
      !! *previous* state as an unsigned value in [0, 2**32).
      class(pcg32_t), intent(inout) :: self
      integer(int64) :: r

      integer(int64) :: old

      old = self%state
      self%state = u64_add(u64_mul(old, PCG_MULTIPLIER), self%increment)
      r = rotate_right32(ibits(ieor(u64_shr(old, 18_default_int), old), 27, 32), &
                         ibits(old, 59, 5))
   end function pcg32_step

   function pcg32_next(self) result(r)
      !! Next 32-bit output, returned as the `int32` with the same bit pattern
      !! as the reference implementation's `uint32_t` result. Roughly half of
      !! the returned values are therefore negative; use `next_real_dp` or
      !! `next_below` if a magnitude is wanted.
      class(pcg32_t), intent(inout) :: self
      integer(int32) :: r

      r = u32_to_int32(pcg32_step(self))
   end function pcg32_next

   function pcg32_next_u64(self) result(r)
      !! Full 64-bit random word built from two consecutive 32-bit outputs; the
      !! first output supplies the high half. Fixing the order here is what makes
      !! `next_real_dp` and `next_below` reproducible for this engine.
      class(pcg32_t), intent(inout) :: self
      integer(int64) :: r

      integer(int64) :: high, low

      high = pcg32_step(self)
      low = pcg32_step(self)
      r = ior(ishft(high, 32), low)
   end function pcg32_next_u64

   function splitmix64_next_real_dp(gen) result(r)
      !! Uniform `real(dp)` in [0, 1) from a SplitMix64 generator.
      type(splitmix64_t), intent(inout) :: gen
      real(dp) :: r

      r = real_from_word(gen%next_u64())
   end function splitmix64_next_real_dp

   function pcg32_next_real_dp(gen) result(r)
      !! Uniform `real(dp)` in [0, 1) from a PCG32 generator. Consumes two
      !! 32-bit outputs.
      type(pcg32_t), intent(inout) :: gen
      real(dp) :: r

      r = real_from_word(gen%next_u64())
   end function pcg32_next_real_dp

   function splitmix64_next_below(gen, n, error) result(r)
      !! Uniform integer in [0, n) from a SplitMix64 generator.
      type(splitmix64_t), intent(inout) :: gen
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: error
      integer(default_int) :: r

      integer(int64) :: bound, mask, candidate

      r = 0_default_int
      if (n <= 0_default_int) then
         if (present(error)) then
            call error%set(ERROR_VALIDATION, "pic_rng: next_below requires n > 0")
         end if
         return
      end if
      if (n == 1_default_int) return

      bound = int(n, int64)
      mask = rejection_mask(bound)
      do
         candidate = iand(gen%next_u64(), mask)
         if (candidate < bound) exit
      end do
      r = int(candidate, default_int)
   end function splitmix64_next_below

   function pcg32_next_below(gen, n, error) result(r)
      !! Uniform integer in [0, n) from a PCG32 generator. Each attempt consumes
      !! two 32-bit outputs, so the stream stays word aligned no matter how many
      !! rejections occur.
      type(pcg32_t), intent(inout) :: gen
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: error
      integer(default_int) :: r

      integer(int64) :: bound, mask, candidate

      r = 0_default_int
      if (n <= 0_default_int) then
         if (present(error)) then
            call error%set(ERROR_VALIDATION, "pic_rng: next_below requires n > 0")
         end if
         return
      end if
      if (n == 1_default_int) return

      bound = int(n, int64)
      mask = rejection_mask(bound)
      do
         candidate = iand(gen%next_u64(), mask)
         if (candidate < bound) exit
      end do
      r = int(candidate, default_int)
   end function pcg32_next_below

   subroutine stream_for_splitmix64(master_seed, stream_id, gen)
      !! Build the SplitMix64 generator belonging to `stream_id`.
      integer(int64), intent(in) :: master_seed
      integer(default_int), intent(in) :: stream_id
      type(splitmix64_t), intent(out) :: gen

      call gen%seed(stream_seed(master_seed, stream_id))
   end subroutine stream_for_splitmix64

   subroutine stream_for_pcg32(master_seed, stream_id, gen)
      !! Build the PCG32 generator belonging to `stream_id`. The derived seed
      !! drives a local SplitMix64 which supplies both the LCG state and the
      !! stream selector.
      integer(int64), intent(in) :: master_seed
      integer(default_int), intent(in) :: stream_id
      type(pcg32_t), intent(out) :: gen

      type(splitmix64_t) :: seeder
      integer(int64) :: initial_state, initial_seq

      call seeder%seed(stream_seed(master_seed, stream_id))
      ! Two separate statements on purpose: Fortran does not specify the order in
      ! which actual arguments are evaluated, so two `seeder%next()` calls inside
      ! one call statement would not be reproducible across compilers.
      initial_state = seeder%next()
      initial_seq = seeder%next()
      call gen%seed(initial_state, initial_seq)
   end subroutine stream_for_pcg32

end module pic_rng
