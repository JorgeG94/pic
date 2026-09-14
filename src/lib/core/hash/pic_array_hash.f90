! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! State hashing of whole intrinsic arrays, for checkpoint validation and
!! cross-run comparison.
module pic_array_hash
   !! A 32- and 64-bit FNV-1a state hash over arrays of intrinsic type.
   !!
   !! `pic_hash_32bit_fnv` already hashes rank-1 integer arrays and character
   !! strings. This module fills the gap needed to fingerprint the state of a
   !! running program: real, complex and logical data, ranks 1 to 3, and a
   !! streaming accumulator so that many arrays -- for example every component
   !! of a derived type -- fold into a single digest.
   !!
   !! ### Quick start
   !!
   !! ```fortran
   !! use pic_array_hash, only: array_hash, array_hash_t, array_hash_hex
   !!
   !! integer(int32) :: fingerprint
   !! type(array_hash_t) :: state
   !!
   !! fingerprint = array_hash(my_matrix)             ! one shot
   !!
   !! call state%update(sim%positions)                ! streaming
   !! call state%update(sim%velocities)
   !! call state%update(sim%step_count)
   !! fingerprint = state%digest()
   !! call logger%info("state " // array_hash_hex(fingerprint))
   !! ```
   !!
   !! ### Supported data
   !!
   !! * `integer` of kind `int8`, `int16`, `int32`, `int64` -- and therefore
   !!   `default_int`, whichever width this build uses -- ranks 0 to 3.
   !! * `real` of kind `sp` and `dp`, ranks 0 to 3.
   !! * `complex` of kind `sp` and `dp`, ranks 0 to 3.
   !! * default `logical`, ranks 0 to 3.
   !! * default `character`: a scalar string, and a rank-1 array of strings.
   !!
   !! Scalars (rank 0) are reachable through `update` only; the one-shot
   !! `array_hash` takes arrays and strings. Rank 4 and higher, and `real(qp)`,
   !! are deliberately not supported; see "Limits" below.
   !!
   !! ### Byte stream definition (stable, and documented on purpose)
   !!
   !! The digest is plain FNV-1a -- 32-bit with offset basis `0x811C9DC5` and
   !! prime `0x01000193`, 64-bit with offset basis `0xCBF29CE484222325` and
   !! prime `0x100000001B3` -- over a byte stream built as follows. The stream
   !! itself is **the same at both widths**: everything below describes one
   !! definition, and the two digests differ only in the FNV parameters folded
   !! over it. Nothing in the stream
   !! depends on host endianness or on the storage layout of any type, so the
   !! same data gives the same digest on every machine and every supported
   !! compiler.
   !!
   !! * An `integer` of encoded width *w* bytes contributes its *w*
   !!   two's-complement bytes, least significant byte first.
   !! * A `logical` contributes one byte: 1 for true, 0 for false.
   !! * A `character` contributes one byte per character, the `iachar` (ASCII)
   !!   code of that character.
   !! * A `real` contributes a fixed 14-byte canonical record: a 1-byte class
   !!   code, a 1-byte sign, a 4-byte binary exponent and an 8-byte mantissa,
   !!   each little endian. See the floating point notes below.
   !! * A `complex` contributes the 14-byte record of its real part followed by
   !!   the 14-byte record of its imaginary part.
   !! * An array contributes the encoding of each element in Fortran array
   !!   element order (column major), with nothing in between.
   !!
   !! ### Floating point: what happens to -0.0, NaN and Inf
   !!
   !! This is the part that bites checkpoint validators, so it is specified
   !! rather than inherited from whatever the bit pattern happens to be. A real
   !! value is **not** hashed by its bits. It is classified first, then
   !! canonicalised:
   !!
   !! * **Positive and negative zero hash identically.** `-0.0` and `+0.0`
   !!   compare equal in Fortran, so a validator built on raw bits would report
   !!   a spurious mismatch between two runs that actually agree. Both are
   !!   encoded as the single class code "zero", with sign, exponent and
   !!   mantissa all zero.
   !! * **Every NaN hashes identically.** All NaNs -- quiet or signalling, of
   !!   either sign, with any payload -- collapse to one class code "NaN". Two
   !!   checkpoints that both went NaN in the same place therefore agree. Note
   !!   the consequence: a NaN never compares equal to itself numerically, but
   !!   it does hash equal to itself here.
   !! * **`+Inf` and `-Inf` are distinguished from each other**, and both are
   !!   distinguished from every finite value and from NaN, because they carry
   !!   separate class codes.
   !! * **Finite values are hashed exactly**, with no rounding and no tolerance.
   !!   The mantissa/exponent decomposition is injective on finite values, so
   !!   two distinct finite numbers always produce distinct records. Subnormals
   !!   are handled like any other finite value.
   !!
   !! There is no raw bitwise entry point. Getting at the bits of a real
   !! requires `transfer` between real and integer kinds, which is precisely
   !! what this module avoids for portability; see "Portability" below.
   !!
   !! Two consequences worth knowing:
   !!
   !! * Because the hash is exact, it answers "are these bit-for-bit the same
   !!   numbers", not "are these the same to within a tolerance". It is a
   !!   reproducibility check, not a physics check.
   !! * `real(sp)` and `real(dp)` holding the same *finite* numeric value hash
   !!   differently, because the mantissa is scaled by the precision of the
   !!   kind. Hash a checkpoint with the kind you stored it in. The zero, NaN
   !!   and infinity records carry no mantissa, so those four values do hash
   !!   the same in `sp` as in `dp`.
   !!
   !! ### Shape sensitivity
   !!
   !! The stream contains element values only; it does not contain the shape.
   !! A 2x3 array, a 3x2 array and a 6-element vector holding the same elements
   !! in the same order therefore all produce the **same** digest. That is the
   !! documented contract, and it is exactly what makes streaming equal to
   !! concatenation.
   !!
   !! When shape must be part of the identity, feed it explicitly first:
   !!
   !! ```fortran
   !! call state%update(shape(a, kind=int32))   ! int32: build independent
   !! call state%update(a)
   !! ```
   !!
   !! Use `kind=int32` there rather than the default, so the digest does not
   !! change between a default-integer build and a `PIC_DEFAULT_INT8` build.
   !!
   !! ### Empty arrays
   !!
   !! A zero-sized array contributes no bytes at all. `update` on one is a
   !! no-op, and the one-shot `array_hash` of one returns
   !! `ARRAY_HASH_OFFSET_BASIS`. Every zero-sized array therefore has the same
   !! digest regardless of its type, kind or rank, and so does a zero-length
   !! string. If "empty" must differ from "absent" in your checkpoint, hash the
   !! size alongside the data as shown above.
   !!
   !! ### Streaming, combining, and the concatenation contract
   !!
   !! `array_hash_t` accumulates. Because the stream is a plain concatenation of
   !! per-element encodings, hashing `a` and then `b` into one accumulator gives
   !! exactly the digest of the concatenated array `[a, b]`. Sub-digests
   !! computed elsewhere -- say, one per MPI rank -- fold in as ordinary int32
   !! data:
   !!
   !! ```fortran
   !! call state%update([rank_digest])
   !! ```
   !!
   !! Order matters: FNV-1a is not commutative, so combine sub-digests in a
   !! fixed, agreed order.
   !!
   !! ### Portability
   !!
   !! No `transfer` between real and integer kinds anywhere; that is the one
   !! construct this module set out to avoid, because it behaves inconsistently
   !! on nvfortran and LFortran. Reals are taken apart arithmetically with the
   !! standard `fraction`, `exponent`, `digits` and `scale` intrinsics, which
   !! are exact for IEEE binary formats. Bytes are extracted with `ibits`, never
   !! with a shift by a negative count. The FNV multiply is carried out in
   !! `int64` and reduced modulo 2**32 by masking; the 64-bit multiply, which
   !! has no wider kind to spill into, is carried out on 16-bit limbs instead
   !! (see `fnv1a_byte64`). Neither can overflow a signed integer. NaN and infinity are detected by comparison (unordered
   !! comparisons against zero, plus `abs(x) > huge(x)`) rather than through
   !! `ieee_arithmetic`, which LFortran does not fully provide.
   !!
   !! That comparison-based classification assumes the compiler has not thrown
   !! IEEE semantics away. Building with `-ffast-math` / `-Ofast`, or
   !! `nvfortran -Mfprelaxed`, lets a compiler assume no NaN can occur and fold
   !! the NaN test away, after which a NaN would be decomposed as though it
   !! were finite. Do not build this module that way.
   !!
   !! ### Limits
   !!
   !! * Ranks 0 to 3 only. Rank 4 and up are reachable today by reshaping to a
   !!   lower rank, which costs nothing in digest terms precisely because the
   !!   hash is shape insensitive.
   !! * `real(qp)` and `complex(qp)` are unsupported: a 113-bit mantissa does
   !!   not fit the `int64` mantissa field, and `qp` is not the same type on
   !!   every target anyway.
   !! * Only default `logical` and default `character` kinds.
   !! * Neither width is a cryptographic checksum, and neither is collision
   !!   resistant against an adversary. 32 bits is fine for *comparing* two
   !!   digests of the same thing, where a false match has probability 2**-32.
   !!   It is not enough to *identify* things: among 10**5 distinct 32-bit
   !!   digests the chance that some pair collides is about 69%, against
   !!   3e-10 at 64 bits. Use `array_hash64` when digests are keys.
   use pic_types, only: default_int, int8, int16, int32, int64, sp, dp
   use pic_uint64, only: u64_add
   implicit none
   private

   public :: array_hash_t
   public :: array_hash
   public :: array_hash_hex

   integer(int32), parameter, public :: ARRAY_HASH_OFFSET_BASIS = -2128831035_int32
      !! The FNV-1a 32-bit offset basis, 0x811C9DC5, written as the signed
      !! `int32` value 2166136261 - 2**32 so that no BOZ literal appears in an
      !! initialiser. This is also the digest of an empty stream. `int32` here
      !! is a fixed-width algorithmic requirement of FNV-1a, not a stand-in for
      !! `default_int`.

   integer(int64), parameter :: FNV_PRIME = 16777619_int64
      !! The FNV-1a 32-bit prime, 0x01000193. Held in `int64` so the FNV
      !! multiply can be done without signed overflow and reduced by masking.
   integer(int64), parameter :: MASK_32 = 4294967295_int64
      !! 2**32 - 1, used to reduce the FNV multiply modulo 2**32.
   integer(int64), parameter :: TWO_POW_32 = 4294967296_int64
      !! 2**32, used to fold an unsigned 32-bit result back to signed `int32`.
   integer(int64), parameter :: INT32_MAX = 2147483647_int64
      !! huge(0_int32); above this an unsigned result folds to a negative one.
   public :: array_hash64_t
   public :: array_hash64
   public :: array_hash64_hex

   integer(int64), parameter, public :: ARRAY_HASH64_OFFSET_BASIS = -3750763034362895579_int64
      !! The FNV-1a 64-bit offset basis, 0xCBF29CE484222325, written as the
      !! signed `int64` value 14695981039346656037 - 2**64 so that no BOZ
      !! literal appears in an initialiser. This is also the digest of an empty
      !! stream. `int64` here is a fixed-width algorithmic requirement of
      !! FNV-1a, not a stand-in for `default_int`.

   integer(int64), parameter :: FNV64_SMALL = 435_int64
      !! The low part of the FNV-1a 64-bit prime. That prime, 0x100000001B3, is
      !! exactly 2**40 + 435, so the multiply modulo 2**64 splits into a shift
      !! and a multiply by this small factor. See `fnv1a_byte64`.
   integer(default_int), parameter :: FNV64_SHIFT = 40_default_int
      !! The high part of the FNV-1a 64-bit prime, as a shift count.
   integer(int64), parameter :: LIMB_MASK = 4294967295_int64
      !! 2**32 - 1. The multiply by `FNV64_SMALL` is carried out on 32-bit
      !! limbs, where no partial product can reach 2**41.
   integer(default_int), parameter :: LIMB_BITS = 32_default_int
      !! Width of one limb of that multiply.

   integer(int64), parameter :: CLASS_ZERO = 0_int64
      !! Class code shared by -0.0 and +0.0.
   integer(int64), parameter :: CLASS_FINITE = 1_int64
      !! Class code for a finite non-zero value.
   integer(int64), parameter :: CLASS_POS_INF = 2_int64
      !! Class code for +Infinity.
   integer(int64), parameter :: CLASS_NEG_INF = 3_int64
      !! Class code for -Infinity.
   integer(int64), parameter :: CLASS_NAN = 4_int64
      !! Class code shared by every NaN, of either sign and any payload.

   integer(int64), parameter :: BYTES_PER_REAL = 14_int64
      !! Width of the canonical real record: 1 class + 1 sign + 4 exponent +
      !! 8 mantissa.

   character(len=16), parameter :: HEX_DIGITS = "0123456789abcdef"
      !! Lookup table for `array_hash_hex`.

   type :: array_hash_t
      !! A streaming 32-bit FNV-1a accumulator over array state.
      !!
      !! A freshly declared `array_hash_t` is already seeded with
      !! `ARRAY_HASH_OFFSET_BASIS`, so no explicit initialisation call is
      !! needed. Feed it with `update`, read it with `digest`, and start over
      !! with `reset`.
      private
      integer(int32) :: state = ARRAY_HASH_OFFSET_BASIS
         !! Running hash. Fixed-width `int32`, because FNV-1a is defined
         !! modulo 2**32.
      integer(int64) :: nbytes = 0_int64
         !! Number of encoded bytes consumed so far. Fixed-width `int64` so
         !! that hashing more than 2 GiB of state cannot overflow the counter
         !! in a `default_int` == `int32` build.
   contains
      procedure :: reset => array_hash_reset
      procedure :: digest => array_hash_digest
      procedure :: bytes_hashed => array_hash_bytes_hashed
      generic :: update => &
         update_int8_0, &
         update_int8_1, &
         update_int8_2, &
         update_int8_3, &
         update_int16_0, &
         update_int16_1, &
         update_int16_2, &
         update_int16_3, &
         update_int32_0, &
         update_int32_1, &
         update_int32_2, &
         update_int32_3, &
         update_int64_0, &
         update_int64_1, &
         update_int64_2, &
         update_int64_3, &
         update_rsp_0, &
         update_rsp_1, &
         update_rsp_2, &
         update_rsp_3, &
         update_rdp_0, &
         update_rdp_1, &
         update_rdp_2, &
         update_rdp_3, &
         update_csp_0, &
         update_csp_1, &
         update_csp_2, &
         update_csp_3, &
         update_cdp_0, &
         update_cdp_1, &
         update_cdp_2, &
         update_cdp_3, &
         update_logical_0, &
         update_logical_1, &
         update_logical_2, &
         update_logical_3, &
         update_char_0, &
         update_char_1

      procedure, private :: update_int8_0
      procedure, private :: update_int8_1
      procedure, private :: update_int8_2
      procedure, private :: update_int8_3
      procedure, private :: update_int16_0
      procedure, private :: update_int16_1
      procedure, private :: update_int16_2
      procedure, private :: update_int16_3
      procedure, private :: update_int32_0
      procedure, private :: update_int32_1
      procedure, private :: update_int32_2
      procedure, private :: update_int32_3
      procedure, private :: update_int64_0
      procedure, private :: update_int64_1
      procedure, private :: update_int64_2
      procedure, private :: update_int64_3
      procedure, private :: update_rsp_0
      procedure, private :: update_rsp_1
      procedure, private :: update_rsp_2
      procedure, private :: update_rsp_3
      procedure, private :: update_rdp_0
      procedure, private :: update_rdp_1
      procedure, private :: update_rdp_2
      procedure, private :: update_rdp_3
      procedure, private :: update_csp_0
      procedure, private :: update_csp_1
      procedure, private :: update_csp_2
      procedure, private :: update_csp_3
      procedure, private :: update_cdp_0
      procedure, private :: update_cdp_1
      procedure, private :: update_cdp_2
      procedure, private :: update_cdp_3
      procedure, private :: update_logical_0
      procedure, private :: update_logical_1
      procedure, private :: update_logical_2
      procedure, private :: update_logical_3
      procedure, private :: update_char_0
      procedure, private :: update_char_1
   end type array_hash_t

   interface array_hash
      !! One-shot digest of a single array or string.
      !!
      !! Equivalent to a fresh `array_hash_t`, one `update` and a `digest`.
      !! Returns `ARRAY_HASH_OFFSET_BASIS` for a zero-sized array or a
      !! zero-length string.
      module procedure :: &
         hash_int8_1, &
         hash_int8_2, &
         hash_int8_3, &
         hash_int16_1, &
         hash_int16_2, &
         hash_int16_3, &
         hash_int32_1, &
         hash_int32_2, &
         hash_int32_3, &
         hash_int64_1, &
         hash_int64_2, &
         hash_int64_3, &
         hash_rsp_1, &
         hash_rsp_2, &
         hash_rsp_3, &
         hash_rdp_1, &
         hash_rdp_2, &
         hash_rdp_3, &
         hash_csp_1, &
         hash_csp_2, &
         hash_csp_3, &
         hash_cdp_1, &
         hash_cdp_2, &
         hash_cdp_3, &
         hash_logical_1, &
         hash_logical_2, &
         hash_logical_3, &
         hash_char_0, &
         hash_char_1
   end interface array_hash
   type :: array_hash64_t
      !! A streaming 64-bit FNV-1a accumulator over array state.
      !!
      !! A freshly declared `array_hash64_t` is already seeded with
      !! `ARRAY_HASH64_OFFSET_BASIS`, so no explicit initialisation call is
      !! needed. Feed it with `update`, read it with `digest`, and start over
      !! with `reset`.
      private
      integer(int64) :: state = ARRAY_HASH64_OFFSET_BASIS
         !! Running hash. Fixed-width `int64`, because FNV-1a is defined
         !! modulo 2**64.
      integer(int64) :: nbytes = 0_int64
         !! Number of encoded bytes consumed so far. Fixed-width `int64` so
         !! that hashing more than 2 GiB of state cannot overflow the counter
         !! in a `default_int` == `int32` build.
   contains
      procedure :: reset => array_hash64_reset
      procedure :: digest => array_hash64_digest
      procedure :: bytes_hashed => array_hash64_bytes_hashed
      generic :: update => &
         update64_int8_0, &
         update64_int8_1, &
         update64_int8_2, &
         update64_int8_3, &
         update64_int16_0, &
         update64_int16_1, &
         update64_int16_2, &
         update64_int16_3, &
         update64_int32_0, &
         update64_int32_1, &
         update64_int32_2, &
         update64_int32_3, &
         update64_int64_0, &
         update64_int64_1, &
         update64_int64_2, &
         update64_int64_3, &
         update64_rsp_0, &
         update64_rsp_1, &
         update64_rsp_2, &
         update64_rsp_3, &
         update64_rdp_0, &
         update64_rdp_1, &
         update64_rdp_2, &
         update64_rdp_3, &
         update64_csp_0, &
         update64_csp_1, &
         update64_csp_2, &
         update64_csp_3, &
         update64_cdp_0, &
         update64_cdp_1, &
         update64_cdp_2, &
         update64_cdp_3, &
         update64_logical_0, &
         update64_logical_1, &
         update64_logical_2, &
         update64_logical_3, &
         update64_char_0, &
         update64_char_1

      procedure, private :: update64_int8_0
      procedure, private :: update64_int8_1
      procedure, private :: update64_int8_2
      procedure, private :: update64_int8_3
      procedure, private :: update64_int16_0
      procedure, private :: update64_int16_1
      procedure, private :: update64_int16_2
      procedure, private :: update64_int16_3
      procedure, private :: update64_int32_0
      procedure, private :: update64_int32_1
      procedure, private :: update64_int32_2
      procedure, private :: update64_int32_3
      procedure, private :: update64_int64_0
      procedure, private :: update64_int64_1
      procedure, private :: update64_int64_2
      procedure, private :: update64_int64_3
      procedure, private :: update64_rsp_0
      procedure, private :: update64_rsp_1
      procedure, private :: update64_rsp_2
      procedure, private :: update64_rsp_3
      procedure, private :: update64_rdp_0
      procedure, private :: update64_rdp_1
      procedure, private :: update64_rdp_2
      procedure, private :: update64_rdp_3
      procedure, private :: update64_csp_0
      procedure, private :: update64_csp_1
      procedure, private :: update64_csp_2
      procedure, private :: update64_csp_3
      procedure, private :: update64_cdp_0
      procedure, private :: update64_cdp_1
      procedure, private :: update64_cdp_2
      procedure, private :: update64_cdp_3
      procedure, private :: update64_logical_0
      procedure, private :: update64_logical_1
      procedure, private :: update64_logical_2
      procedure, private :: update64_logical_3
      procedure, private :: update64_char_0
      procedure, private :: update64_char_1
   end type array_hash64_t

   interface array_hash64
      !! One-shot digest of a single array or string.
      !!
      !! Equivalent to a fresh `array_hash64_t`, one `update` and a `digest`.
      !! Returns `ARRAY_HASH64_OFFSET_BASIS` for a zero-sized array or a
      !! zero-length string.
      module procedure :: &
         hash64_int8_1, &
         hash64_int8_2, &
         hash64_int8_3, &
         hash64_int16_1, &
         hash64_int16_2, &
         hash64_int16_3, &
         hash64_int32_1, &
         hash64_int32_2, &
         hash64_int32_3, &
         hash64_int64_1, &
         hash64_int64_2, &
         hash64_int64_3, &
         hash64_rsp_1, &
         hash64_rsp_2, &
         hash64_rsp_3, &
         hash64_rdp_1, &
         hash64_rdp_2, &
         hash64_rdp_3, &
         hash64_csp_1, &
         hash64_csp_2, &
         hash64_csp_3, &
         hash64_cdp_1, &
         hash64_cdp_2, &
         hash64_cdp_3, &
         hash64_logical_1, &
         hash64_logical_2, &
         hash64_logical_3, &
         hash64_char_0, &
         hash64_char_1
   end interface array_hash64

contains

   pure subroutine array_hash_reset(this, seed)
      !! Return the accumulator to its initial state.
      !!
      !! With no argument it restarts from `ARRAY_HASH_OFFSET_BASIS`. Pass
      !! `seed` to start from a different value instead, which is how you
      !! domain-separate two hashes over the same bytes, or resume a digest
      !! saved by an earlier run.
      class(array_hash_t), intent(inout) :: this
      integer(int32), intent(in), optional :: seed
         !! Starting hash value; defaults to `ARRAY_HASH_OFFSET_BASIS`.

      if (present(seed)) then
         this%state = seed
      else
         this%state = ARRAY_HASH_OFFSET_BASIS
      end if
      this%nbytes = 0_int64
   end subroutine array_hash_reset

   pure function array_hash_digest(this) result(hash_value)
      !! The digest of everything fed in so far.
      !!
      !! Reading it does not consume the state, so it may be taken at any point
      !! and the accumulator updated further afterwards.
      class(array_hash_t), intent(in) :: this
      integer(int32) :: hash_value

      hash_value = this%state
   end function array_hash_digest

   pure function array_hash_bytes_hashed(this) result(nbytes)
      !! Number of encoded bytes consumed so far.
      !!
      !! This counts the encoded stream, not the storage size of the input:
      !! each real costs 14 bytes and each complex 28, whatever the kind. It is
      !! a cheap sanity check that a checkpoint walk visited everything it was
      !! supposed to visit.
      class(array_hash_t), intent(in) :: this
      integer(int64) :: nbytes

      nbytes = this%nbytes
   end function array_hash_bytes_hashed

   pure function array_hash_hex(hash_value) result(text)
      !! Format a digest as 8 lowercase hexadecimal characters.
      !!
      !! Built from `ibits` and a lookup table rather than a `Z` edit
      !! descriptor, so the result is identical on every compiler and no
      !! internal I/O is involved.
      integer(int32), intent(in) :: hash_value
         !! Digest to format, typically from `digest` or `array_hash`.
      character(len=8) :: text

      integer(default_int) :: i, nibble

      do i = 1_default_int, 8_default_int
         nibble = int(ibits(hash_value, 4_default_int*(8_default_int - i), &
                            4_default_int), default_int)
         text(i:i) = HEX_DIGITS(nibble + 1_default_int:nibble + 1_default_int)
      end do
   end function array_hash_hex

   pure subroutine fnv1a_byte(state, byte_value)
      !! One FNV-1a round: exclusive-or in a byte, then multiply by the prime.
      !!
      !! The multiply is done in `int64` and reduced modulo 2**32 by masking,
      !! then folded back into the signed `int32` range. Doing it directly in
      !! `int32` would overflow, which Fortran leaves undefined and which some
      !! compilers trap on.
      integer(int32), intent(inout) :: state
      integer(int64), intent(in) :: byte_value
         !! A single byte, 0 to 255.

      integer(int64) :: acc

      acc = int(ieor(state, int(byte_value, int32)), int64)
      acc = iand(acc*FNV_PRIME, MASK_32)
      if (acc > INT32_MAX) acc = acc - TWO_POW_32
      state = int(acc, int32)
   end subroutine fnv1a_byte

   pure subroutine feed_integer(state, value, nbytes)
      !! Feed the `nbytes` low-order bytes of `value`, least significant first.
      !!
      !! `ibits` yields the unsigned value of each byte of the two's-complement
      !! representation, so the stream is independent of host endianness.
      integer(int32), intent(inout) :: state
      integer(int64), intent(in) :: value
         !! Value to encode, already widened to `int64` by the caller.
      integer(default_int), intent(in) :: nbytes
         !! How many low-order bytes of `value` are significant, 1 to 8.

      integer(default_int) :: i

      do i = 0_default_int, nbytes - 1_default_int
         call fnv1a_byte(state, ibits(value, 8_default_int*i, 8_default_int))
      end do
   end subroutine feed_integer

   pure subroutine feed_logical(state, value)
      !! Feed one byte: 1 for true, 0 for false.
      integer(int32), intent(inout) :: state
      logical, intent(in) :: value
         !! Value to encode.

      if (value) then
         call feed_integer(state, 1_int64, 1_default_int)
      else
         call feed_integer(state, 0_int64, 1_default_int)
      end if
   end subroutine feed_logical

   pure subroutine feed_real_record(state, class_code, sign_code, expo, mantissa)
      !! Feed the fixed 14-byte canonical record of one real value.
      integer(int32), intent(inout) :: state
      integer(int64), intent(in) :: class_code
         !! One of the CLASS_* codes.
      integer(int64), intent(in) :: sign_code
         !! 1 for a negative finite value, 0 otherwise.
      integer(int64), intent(in) :: expo
         !! Binary exponent; 0 unless the value is finite and non-zero.
      integer(int64), intent(in) :: mantissa
         !! Scaled integer mantissa; 0 unless the value is finite and non-zero.

      call feed_integer(state, class_code, 1_default_int)
      call feed_integer(state, sign_code, 1_default_int)
      call feed_integer(state, expo, 4_default_int)
      call feed_integer(state, mantissa, 8_default_int)
   end subroutine feed_real_record

   pure subroutine feed_real_sp(state, value)
      !! Canonicalise and feed one `real(sp)` value.
      !!
      !! NaN is tested first, as the one value that is unordered with respect
      !! to zero: neither `value >= 0` nor `value <= 0` holds. That ordered
      !! form is used in preference to the more familiar `value /= value`
      !! because it says the same thing without an equality test on a real,
      !! which every compiler warns about. Infinities are then separated from
      !! finite values by comparison against `huge`, so that `exponent` and
      !! `fraction` are only ever applied to a finite, non-zero argument, and
      !! zero is finally detected as the value that is not greater than zero in
      !! magnitude -- which is true for -0.0 as well as +0.0, and is where the
      !! two signed zeroes become one.
      integer(int32), intent(inout) :: state
      real(sp), intent(in) :: value
         !! Value to encode; may be any value including -0.0, NaN and +/-Inf.

      integer(int64) :: class_code, sign_code, expo, mantissa

      sign_code = 0_int64
      expo = 0_int64
      mantissa = 0_int64
      if (.not. (value >= 0.0_sp .or. value <= 0.0_sp)) then
         class_code = CLASS_NAN
      else if (value > huge(value)) then
         class_code = CLASS_POS_INF
      else if (value < -huge(value)) then
         class_code = CLASS_NEG_INF
      else if (.not. (abs(value) > 0.0_sp)) then
         class_code = CLASS_ZERO
      else
         class_code = CLASS_FINITE
         if (value < 0.0_sp) sign_code = 1_int64
         expo = int(exponent(value), int64)
         mantissa = int(scale(fraction(abs(value)), digits(value)), int64)
      end if
      call feed_real_record(state, class_code, sign_code, expo, mantissa)
   end subroutine feed_real_sp

   pure subroutine feed_real_dp(state, value)
      !! Canonicalise and feed one `real(dp)` value. See `feed_real_sp` for the
      !! ordering of the classification tests.
      integer(int32), intent(inout) :: state
      real(dp), intent(in) :: value
         !! Value to encode; may be any value including -0.0, NaN and +/-Inf.

      integer(int64) :: class_code, sign_code, expo, mantissa

      sign_code = 0_int64
      expo = 0_int64
      mantissa = 0_int64
      if (.not. (value >= 0.0_dp .or. value <= 0.0_dp)) then
         class_code = CLASS_NAN
      else if (value > huge(value)) then
         class_code = CLASS_POS_INF
      else if (value < -huge(value)) then
         class_code = CLASS_NEG_INF
      else if (.not. (abs(value) > 0.0_dp)) then
         class_code = CLASS_ZERO
      else
         class_code = CLASS_FINITE
         if (value < 0.0_dp) sign_code = 1_int64
         expo = int(exponent(value), int64)
         mantissa = int(scale(fraction(abs(value)), digits(value)), int64)
      end if
      call feed_real_record(state, class_code, sign_code, expo, mantissa)
   end subroutine feed_real_dp

   ! ---- update: one specific per type, kind and rank ----

   pure subroutine update_int8_0(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int8), intent(in) :: key

      call feed_integer(this%state, int(key, int64), 1_default_int)
      this%nbytes = this%nbytes + 1_int64
   end subroutine update_int8_0

   pure subroutine update_int8_1(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer(this%state, int(key(i), int64), 1_default_int)
      end do
      this%nbytes = this%nbytes + (1_int64)*size(key, kind=int64)
   end subroutine update_int8_1

   pure subroutine update_int8_2(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_int8_2

   pure subroutine update_int8_3(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_int8_3

   pure subroutine update_int16_0(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int16), intent(in) :: key

      call feed_integer(this%state, int(key, int64), 2_default_int)
      this%nbytes = this%nbytes + 2_int64
   end subroutine update_int16_0

   pure subroutine update_int16_1(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer(this%state, int(key(i), int64), 2_default_int)
      end do
      this%nbytes = this%nbytes + (2_int64)*size(key, kind=int64)
   end subroutine update_int16_1

   pure subroutine update_int16_2(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_int16_2

   pure subroutine update_int16_3(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_int16_3

   pure subroutine update_int32_0(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int32), intent(in) :: key

      call feed_integer(this%state, int(key, int64), 4_default_int)
      this%nbytes = this%nbytes + 4_int64
   end subroutine update_int32_0

   pure subroutine update_int32_1(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer(this%state, int(key(i), int64), 4_default_int)
      end do
      this%nbytes = this%nbytes + (4_int64)*size(key, kind=int64)
   end subroutine update_int32_1

   pure subroutine update_int32_2(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_int32_2

   pure subroutine update_int32_3(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_int32_3

   pure subroutine update_int64_0(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int64), intent(in) :: key

      call feed_integer(this%state, key, 8_default_int)
      this%nbytes = this%nbytes + 8_int64
   end subroutine update_int64_0

   pure subroutine update_int64_1(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer(this%state, key(i), 8_default_int)
      end do
      this%nbytes = this%nbytes + (8_int64)*size(key, kind=int64)
   end subroutine update_int64_1

   pure subroutine update_int64_2(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_int64_2

   pure subroutine update_int64_3(this, key)
      class(array_hash_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_int64_3

   pure subroutine update_rsp_0(this, key)
      class(array_hash_t), intent(inout) :: this
      real(sp), intent(in) :: key

      call feed_real_sp(this%state, key)
      this%nbytes = this%nbytes + BYTES_PER_REAL
   end subroutine update_rsp_0

   pure subroutine update_rsp_1(this, key)
      class(array_hash_t), intent(inout) :: this
      real(sp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_sp(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update_rsp_1

   pure subroutine update_rsp_2(this, key)
      class(array_hash_t), intent(inout) :: this
      real(sp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_rsp_2

   pure subroutine update_rsp_3(this, key)
      class(array_hash_t), intent(inout) :: this
      real(sp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_rsp_3

   pure subroutine update_rdp_0(this, key)
      class(array_hash_t), intent(inout) :: this
      real(dp), intent(in) :: key

      call feed_real_dp(this%state, key)
      this%nbytes = this%nbytes + BYTES_PER_REAL
   end subroutine update_rdp_0

   pure subroutine update_rdp_1(this, key)
      class(array_hash_t), intent(inout) :: this
      real(dp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_dp(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update_rdp_1

   pure subroutine update_rdp_2(this, key)
      class(array_hash_t), intent(inout) :: this
      real(dp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_rdp_2

   pure subroutine update_rdp_3(this, key)
      class(array_hash_t), intent(inout) :: this
      real(dp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_rdp_3

   pure subroutine update_csp_0(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(sp), intent(in) :: key

      call feed_real_sp(this%state, real(key, sp))
      call feed_real_sp(this%state, aimag(key))
      this%nbytes = this%nbytes + 2_int64*BYTES_PER_REAL
   end subroutine update_csp_0

   pure subroutine update_csp_1(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_sp(this%state, real(key(i), sp))
         call feed_real_sp(this%state, aimag(key(i)))
      end do
      this%nbytes = this%nbytes + (2_int64*BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update_csp_1

   pure subroutine update_csp_2(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_csp_2

   pure subroutine update_csp_3(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_csp_3

   pure subroutine update_cdp_0(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(dp), intent(in) :: key

      call feed_real_dp(this%state, real(key, dp))
      call feed_real_dp(this%state, aimag(key))
      this%nbytes = this%nbytes + 2_int64*BYTES_PER_REAL
   end subroutine update_cdp_0

   pure subroutine update_cdp_1(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_dp(this%state, real(key(i), dp))
         call feed_real_dp(this%state, aimag(key(i)))
      end do
      this%nbytes = this%nbytes + (2_int64*BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update_cdp_1

   pure subroutine update_cdp_2(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_cdp_2

   pure subroutine update_cdp_3(this, key)
      class(array_hash_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_cdp_3

   pure subroutine update_logical_0(this, key)
      class(array_hash_t), intent(inout) :: this
      logical, intent(in) :: key

      call feed_logical(this%state, key)
      this%nbytes = this%nbytes + 1_int64
   end subroutine update_logical_0

   pure subroutine update_logical_1(this, key)
      class(array_hash_t), intent(inout) :: this
      logical, intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_logical(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (1_int64)*size(key, kind=int64)
   end subroutine update_logical_1

   pure subroutine update_logical_2(this, key)
      class(array_hash_t), intent(inout) :: this
      logical, intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update_logical_2

   pure subroutine update_logical_3(this, key)
      class(array_hash_t), intent(inout) :: this
      logical, intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update_logical_3

   pure subroutine update_char_0(this, key)
      !! Feed a character string, one `iachar` byte per character.
      class(array_hash_t), intent(inout) :: this
      character(len=*), intent(in) :: key

      integer(default_int) :: i

      do i = 1_default_int, len(key, kind=default_int)
         call feed_integer(this%state, int(iachar(key(i:i)), int64), 1_default_int)
      end do
      this%nbytes = this%nbytes + len(key, kind=int64)
   end subroutine update_char_0

   pure subroutine update_char_1(this, key)
      class(array_hash_t), intent(inout) :: this
      character(len=*), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call this%update(key(i))
      end do
   end subroutine update_char_1

   ! ---- array_hash: one-shot wrappers ----

   pure function hash_int8_1(key) result(hash_value)
      integer(int8), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int8_1

   pure function hash_int8_2(key) result(hash_value)
      integer(int8), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int8_2

   pure function hash_int8_3(key) result(hash_value)
      integer(int8), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int8_3

   pure function hash_int16_1(key) result(hash_value)
      integer(int16), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int16_1

   pure function hash_int16_2(key) result(hash_value)
      integer(int16), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int16_2

   pure function hash_int16_3(key) result(hash_value)
      integer(int16), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int16_3

   pure function hash_int32_1(key) result(hash_value)
      integer(int32), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int32_1

   pure function hash_int32_2(key) result(hash_value)
      integer(int32), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int32_2

   pure function hash_int32_3(key) result(hash_value)
      integer(int32), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int32_3

   pure function hash_int64_1(key) result(hash_value)
      integer(int64), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int64_1

   pure function hash_int64_2(key) result(hash_value)
      integer(int64), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int64_2

   pure function hash_int64_3(key) result(hash_value)
      integer(int64), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_int64_3

   pure function hash_rsp_1(key) result(hash_value)
      real(sp), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rsp_1

   pure function hash_rsp_2(key) result(hash_value)
      real(sp), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rsp_2

   pure function hash_rsp_3(key) result(hash_value)
      real(sp), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rsp_3

   pure function hash_rdp_1(key) result(hash_value)
      real(dp), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rdp_1

   pure function hash_rdp_2(key) result(hash_value)
      real(dp), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rdp_2

   pure function hash_rdp_3(key) result(hash_value)
      real(dp), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_rdp_3

   pure function hash_csp_1(key) result(hash_value)
      complex(sp), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_csp_1

   pure function hash_csp_2(key) result(hash_value)
      complex(sp), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_csp_2

   pure function hash_csp_3(key) result(hash_value)
      complex(sp), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_csp_3

   pure function hash_cdp_1(key) result(hash_value)
      complex(dp), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_cdp_1

   pure function hash_cdp_2(key) result(hash_value)
      complex(dp), intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_cdp_2

   pure function hash_cdp_3(key) result(hash_value)
      complex(dp), intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_cdp_3

   pure function hash_logical_1(key) result(hash_value)
      logical, intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_logical_1

   pure function hash_logical_2(key) result(hash_value)
      logical, intent(in) :: key(:, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_logical_2

   pure function hash_logical_3(key) result(hash_value)
      logical, intent(in) :: key(:, :, :)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_logical_3

   pure function hash_char_0(key) result(hash_value)
      character(len=*), intent(in) :: key
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_char_0

   pure function hash_char_1(key) result(hash_value)
      character(len=*), intent(in) :: key(:)
      integer(int32) :: hash_value

      type(array_hash_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash_char_1

   pure subroutine array_hash64_reset(this, seed)
      !! Return the accumulator to its initial state.
      !!
      !! With no argument it restarts from `ARRAY_HASH64_OFFSET_BASIS`. Pass
      !! `seed` to start from a different value instead, which is how you
      !! domain-separate two hashes over the same bytes, or resume a digest
      !! saved by an earlier run.
      class(array_hash64_t), intent(inout) :: this
      integer(int64), intent(in), optional :: seed
         !! Starting hash value; defaults to `ARRAY_HASH64_OFFSET_BASIS`.

      if (present(seed)) then
         this%state = seed
      else
         this%state = ARRAY_HASH64_OFFSET_BASIS
      end if
      this%nbytes = 0_int64
   end subroutine array_hash64_reset

   pure function array_hash64_digest(this) result(hash_value)
      !! The digest of everything fed in so far.
      !!
      !! Reading it does not consume the state, so it may be taken at any point
      !! and the accumulator updated further afterwards.
      class(array_hash64_t), intent(in) :: this
      integer(int64) :: hash_value

      hash_value = this%state
   end function array_hash64_digest

   pure function array_hash64_bytes_hashed(this) result(nbytes)
      !! Number of encoded bytes consumed so far.
      !!
      !! This counts the encoded stream, not the storage size of the input:
      !! each real costs 14 bytes and each complex 28, whatever the kind. It is
      !! a cheap sanity check that a checkpoint walk visited everything it was
      !! supposed to visit.
      class(array_hash64_t), intent(in) :: this
      integer(int64) :: nbytes

      nbytes = this%nbytes
   end function array_hash64_bytes_hashed

   pure function array_hash64_hex(hash_value) result(text)
      !! Format a digest as 16 lowercase hexadecimal characters.
      !!
      !! Built from `ibits` and a lookup table rather than a `Z` edit
      !! descriptor, so the result is identical on every compiler and no
      !! internal I/O is involved.
      integer(int64), intent(in) :: hash_value
         !! Digest to format, typically from `digest` or `array_hash64`.
      character(len=16) :: text

      integer(default_int) :: i, nibble

      do i = 1_default_int, 16_default_int
         nibble = int(ibits(hash_value, 4_default_int*(16_default_int - i), &
                            4_default_int), default_int)
         text(i:i) = HEX_DIGITS(nibble + 1_default_int:nibble + 1_default_int)
      end do
   end function array_hash64_hex

   pure subroutine fnv1a_byte64(state, byte_value)
      !! One FNV-1a round: exclusive-or in a byte, then multiply by the prime.
      !!
      !! There is no integer kind wider than `int64` to compute the product in
      !! and mask back, the way the 32-bit round does. Instead the multiply is
      !! done modulo 2**64 directly, using the structure of the prime:
      !! 0x100000001B3 is exactly 2**40 + 435, so
      !!
      !! ```
      !! h * prime  =  (h << 40)  +  h * 435        (mod 2**64)
      !! ```
      !!
      !! The shift discards the bits that leave the top, which is the reduction
      !! modulo 2**64. The remaining `h * 435` is formed on two 32-bit limbs.
      !! Each partial product is below 2**41, so neither can overflow a signed
      !! `int64`, and no carry has to be propagated between them: the low
      !! limb's product is added in whole, and the high limb's contributes only
      !! the 32 bits that have not already left the top of the word. Both
      !! additions go through `u64_add`, which wraps rather than overflowing.
      !!
      !! Two multiplies per byte, against the ten a general `u64_mul` would
      !! cost. A 16-bit-limb version with carries is also exact but needs four,
      !! and measured 2.18x the cost of the 32-bit hash per byte where this one
      !! measures 1.77x (gfortran -O2, 64 MiB, best of three).
      integer(int64), intent(inout) :: state
      integer(int64), intent(in) :: byte_value
         !! A single byte, 0 to 255.

      integer(int64) :: h, low, high

      h = ieor(state, byte_value)

      low = iand(h, LIMB_MASK)*FNV64_SMALL
      high = iand(ishft(h, -LIMB_BITS), LIMB_MASK)*FNV64_SMALL

      ! The high limb's product is shifted back up, which drops the bits that
      ! leave the top -- part of the same reduction modulo 2**64. `ishft` is
      ! used rather than a multiply because the result may set bit 63, and
      ! reaching that bit arithmetically would be signed overflow.
      state = u64_add(ishft(h, FNV64_SHIFT), &
                      u64_add(low, ishft(iand(high, LIMB_MASK), LIMB_BITS)))
   end subroutine fnv1a_byte64

   pure subroutine feed_integer64(state, value, nbytes)
      !! Feed the `nbytes` low-order bytes of `value`, least significant first.
      !!
      !! `ibits` yields the unsigned value of each byte of the two's-complement
      !! representation, so the stream is independent of host endianness.
      integer(int64), intent(inout) :: state
      integer(int64), intent(in) :: value
         !! Value to encode, already widened to `int64` by the caller.
      integer(default_int), intent(in) :: nbytes
         !! How many low-order bytes of `value` are significant, 1 to 8.

      integer(default_int) :: i

      do i = 0_default_int, nbytes - 1_default_int
         call fnv1a_byte64(state, ibits(value, 8_default_int*i, 8_default_int))
      end do
   end subroutine feed_integer64

   pure subroutine feed_logical64(state, value)
      !! Feed one byte: 1 for true, 0 for false.
      integer(int64), intent(inout) :: state
      logical, intent(in) :: value
         !! Value to encode.

      if (value) then
         call feed_integer64(state, 1_int64, 1_default_int)
      else
         call feed_integer64(state, 0_int64, 1_default_int)
      end if
   end subroutine feed_logical64

   pure subroutine feed_real_record64(state, class_code, sign_code, expo, mantissa)
      !! Feed the fixed 14-byte canonical record of one real value.
      integer(int64), intent(inout) :: state
      integer(int64), intent(in) :: class_code
         !! One of the CLASS_* codes.
      integer(int64), intent(in) :: sign_code
         !! 1 for a negative finite value, 0 otherwise.
      integer(int64), intent(in) :: expo
         !! Binary exponent; 0 unless the value is finite and non-zero.
      integer(int64), intent(in) :: mantissa
         !! Scaled integer mantissa; 0 unless the value is finite and non-zero.

      call feed_integer64(state, class_code, 1_default_int)
      call feed_integer64(state, sign_code, 1_default_int)
      call feed_integer64(state, expo, 4_default_int)
      call feed_integer64(state, mantissa, 8_default_int)
   end subroutine feed_real_record64

   pure subroutine feed_real_sp64(state, value)
      !! Canonicalise and feed one `real(sp)` value.
      !!
      !! NaN is tested first, as the one value that is unordered with respect
      !! to zero: neither `value >= 0` nor `value <= 0` holds. That ordered
      !! form is used in preference to the more familiar `value /= value`
      !! because it says the same thing without an equality test on a real,
      !! which every compiler warns about. Infinities are then separated from
      !! finite values by comparison against `huge`, so that `exponent` and
      !! `fraction` are only ever applied to a finite, non-zero argument, and
      !! zero is finally detected as the value that is not greater than zero in
      !! magnitude -- which is true for -0.0 as well as +0.0, and is where the
      !! two signed zeroes become one.
      integer(int64), intent(inout) :: state
      real(sp), intent(in) :: value
         !! Value to encode; may be any value including -0.0, NaN and +/-Inf.

      integer(int64) :: class_code, sign_code, expo, mantissa

      sign_code = 0_int64
      expo = 0_int64
      mantissa = 0_int64
      if (.not. (value >= 0.0_sp .or. value <= 0.0_sp)) then
         class_code = CLASS_NAN
      else if (value > huge(value)) then
         class_code = CLASS_POS_INF
      else if (value < -huge(value)) then
         class_code = CLASS_NEG_INF
      else if (.not. (abs(value) > 0.0_sp)) then
         class_code = CLASS_ZERO
      else
         class_code = CLASS_FINITE
         if (value < 0.0_sp) sign_code = 1_int64
         expo = int(exponent(value), int64)
         mantissa = int(scale(fraction(abs(value)), digits(value)), int64)
      end if
      call feed_real_record64(state, class_code, sign_code, expo, mantissa)
   end subroutine feed_real_sp64

   pure subroutine feed_real_dp64(state, value)
      !! Canonicalise and feed one `real(dp)` value. See `feed_real_sp64` for the
      !! ordering of the classification tests.
      integer(int64), intent(inout) :: state
      real(dp), intent(in) :: value
         !! Value to encode; may be any value including -0.0, NaN and +/-Inf.

      integer(int64) :: class_code, sign_code, expo, mantissa

      sign_code = 0_int64
      expo = 0_int64
      mantissa = 0_int64
      if (.not. (value >= 0.0_dp .or. value <= 0.0_dp)) then
         class_code = CLASS_NAN
      else if (value > huge(value)) then
         class_code = CLASS_POS_INF
      else if (value < -huge(value)) then
         class_code = CLASS_NEG_INF
      else if (.not. (abs(value) > 0.0_dp)) then
         class_code = CLASS_ZERO
      else
         class_code = CLASS_FINITE
         if (value < 0.0_dp) sign_code = 1_int64
         expo = int(exponent(value), int64)
         mantissa = int(scale(fraction(abs(value)), digits(value)), int64)
      end if
      call feed_real_record64(state, class_code, sign_code, expo, mantissa)
   end subroutine feed_real_dp64

   ! ---- update: one specific per type, kind and rank ----

   pure subroutine update64_int8_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int8), intent(in) :: key

      call feed_integer64(this%state, int(key, int64), 1_default_int)
      this%nbytes = this%nbytes + 1_int64
   end subroutine update64_int8_0

   pure subroutine update64_int8_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer64(this%state, int(key(i), int64), 1_default_int)
      end do
      this%nbytes = this%nbytes + (1_int64)*size(key, kind=int64)
   end subroutine update64_int8_1

   pure subroutine update64_int8_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_int8_2

   pure subroutine update64_int8_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int8), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_int8_3

   pure subroutine update64_int16_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int16), intent(in) :: key

      call feed_integer64(this%state, int(key, int64), 2_default_int)
      this%nbytes = this%nbytes + 2_int64
   end subroutine update64_int16_0

   pure subroutine update64_int16_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer64(this%state, int(key(i), int64), 2_default_int)
      end do
      this%nbytes = this%nbytes + (2_int64)*size(key, kind=int64)
   end subroutine update64_int16_1

   pure subroutine update64_int16_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_int16_2

   pure subroutine update64_int16_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int16), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_int16_3

   pure subroutine update64_int32_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int32), intent(in) :: key

      call feed_integer64(this%state, int(key, int64), 4_default_int)
      this%nbytes = this%nbytes + 4_int64
   end subroutine update64_int32_0

   pure subroutine update64_int32_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer64(this%state, int(key(i), int64), 4_default_int)
      end do
      this%nbytes = this%nbytes + (4_int64)*size(key, kind=int64)
   end subroutine update64_int32_1

   pure subroutine update64_int32_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_int32_2

   pure subroutine update64_int32_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int32), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_int32_3

   pure subroutine update64_int64_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int64), intent(in) :: key

      call feed_integer64(this%state, key, 8_default_int)
      this%nbytes = this%nbytes + 8_int64
   end subroutine update64_int64_0

   pure subroutine update64_int64_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_integer64(this%state, key(i), 8_default_int)
      end do
      this%nbytes = this%nbytes + (8_int64)*size(key, kind=int64)
   end subroutine update64_int64_1

   pure subroutine update64_int64_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_int64_2

   pure subroutine update64_int64_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      integer(int64), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_int64_3

   pure subroutine update64_rsp_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(sp), intent(in) :: key

      call feed_real_sp64(this%state, key)
      this%nbytes = this%nbytes + BYTES_PER_REAL
   end subroutine update64_rsp_0

   pure subroutine update64_rsp_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(sp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_sp64(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update64_rsp_1

   pure subroutine update64_rsp_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(sp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_rsp_2

   pure subroutine update64_rsp_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(sp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_rsp_3

   pure subroutine update64_rdp_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(dp), intent(in) :: key

      call feed_real_dp64(this%state, key)
      this%nbytes = this%nbytes + BYTES_PER_REAL
   end subroutine update64_rdp_0

   pure subroutine update64_rdp_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(dp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_dp64(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update64_rdp_1

   pure subroutine update64_rdp_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(dp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_rdp_2

   pure subroutine update64_rdp_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      real(dp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_rdp_3

   pure subroutine update64_csp_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(sp), intent(in) :: key

      call feed_real_sp64(this%state, real(key, sp))
      call feed_real_sp64(this%state, aimag(key))
      this%nbytes = this%nbytes + 2_int64*BYTES_PER_REAL
   end subroutine update64_csp_0

   pure subroutine update64_csp_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_sp64(this%state, real(key(i), sp))
         call feed_real_sp64(this%state, aimag(key(i)))
      end do
      this%nbytes = this%nbytes + (2_int64*BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update64_csp_1

   pure subroutine update64_csp_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_csp_2

   pure subroutine update64_csp_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(sp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_csp_3

   pure subroutine update64_cdp_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(dp), intent(in) :: key

      call feed_real_dp64(this%state, real(key, dp))
      call feed_real_dp64(this%state, aimag(key))
      this%nbytes = this%nbytes + 2_int64*BYTES_PER_REAL
   end subroutine update64_cdp_0

   pure subroutine update64_cdp_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_real_dp64(this%state, real(key(i), dp))
         call feed_real_dp64(this%state, aimag(key(i)))
      end do
      this%nbytes = this%nbytes + (2_int64*BYTES_PER_REAL)*size(key, kind=int64)
   end subroutine update64_cdp_1

   pure subroutine update64_cdp_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_cdp_2

   pure subroutine update64_cdp_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      complex(dp), intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_cdp_3

   pure subroutine update64_logical_0(this, key)
      class(array_hash64_t), intent(inout) :: this
      logical, intent(in) :: key

      call feed_logical64(this%state, key)
      this%nbytes = this%nbytes + 1_int64
   end subroutine update64_logical_0

   pure subroutine update64_logical_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      logical, intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call feed_logical64(this%state, key(i))
      end do
      this%nbytes = this%nbytes + (1_int64)*size(key, kind=int64)
   end subroutine update64_logical_1

   pure subroutine update64_logical_2(this, key)
      class(array_hash64_t), intent(inout) :: this
      logical, intent(in) :: key(:, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 2, kind=default_int)
         call this%update(key(:, i))
      end do
   end subroutine update64_logical_2

   pure subroutine update64_logical_3(this, key)
      class(array_hash64_t), intent(inout) :: this
      logical, intent(in) :: key(:, :, :)

      integer(default_int) :: i

      do i = 1_default_int, size(key, 3, kind=default_int)
         call this%update(key(:, :, i))
      end do
   end subroutine update64_logical_3

   pure subroutine update64_char_0(this, key)
      !! Feed a character string, one `iachar` byte per character.
      class(array_hash64_t), intent(inout) :: this
      character(len=*), intent(in) :: key

      integer(default_int) :: i

      do i = 1_default_int, len(key, kind=default_int)
         call feed_integer64(this%state, int(iachar(key(i:i)), int64), 1_default_int)
      end do
      this%nbytes = this%nbytes + len(key, kind=int64)
   end subroutine update64_char_0

   pure subroutine update64_char_1(this, key)
      class(array_hash64_t), intent(inout) :: this
      character(len=*), intent(in) :: key(:)

      integer(default_int) :: i

      do i = 1_default_int, size(key, kind=default_int)
         call this%update(key(i))
      end do
   end subroutine update64_char_1

   ! ---- array_hash64: one-shot wrappers ----

   pure function hash64_int8_1(key) result(hash_value)
      integer(int8), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int8_1

   pure function hash64_int8_2(key) result(hash_value)
      integer(int8), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int8_2

   pure function hash64_int8_3(key) result(hash_value)
      integer(int8), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int8_3

   pure function hash64_int16_1(key) result(hash_value)
      integer(int16), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int16_1

   pure function hash64_int16_2(key) result(hash_value)
      integer(int16), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int16_2

   pure function hash64_int16_3(key) result(hash_value)
      integer(int16), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int16_3

   pure function hash64_int32_1(key) result(hash_value)
      integer(int32), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int32_1

   pure function hash64_int32_2(key) result(hash_value)
      integer(int32), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int32_2

   pure function hash64_int32_3(key) result(hash_value)
      integer(int32), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int32_3

   pure function hash64_int64_1(key) result(hash_value)
      integer(int64), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int64_1

   pure function hash64_int64_2(key) result(hash_value)
      integer(int64), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int64_2

   pure function hash64_int64_3(key) result(hash_value)
      integer(int64), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_int64_3

   pure function hash64_rsp_1(key) result(hash_value)
      real(sp), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rsp_1

   pure function hash64_rsp_2(key) result(hash_value)
      real(sp), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rsp_2

   pure function hash64_rsp_3(key) result(hash_value)
      real(sp), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rsp_3

   pure function hash64_rdp_1(key) result(hash_value)
      real(dp), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rdp_1

   pure function hash64_rdp_2(key) result(hash_value)
      real(dp), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rdp_2

   pure function hash64_rdp_3(key) result(hash_value)
      real(dp), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_rdp_3

   pure function hash64_csp_1(key) result(hash_value)
      complex(sp), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_csp_1

   pure function hash64_csp_2(key) result(hash_value)
      complex(sp), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_csp_2

   pure function hash64_csp_3(key) result(hash_value)
      complex(sp), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_csp_3

   pure function hash64_cdp_1(key) result(hash_value)
      complex(dp), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_cdp_1

   pure function hash64_cdp_2(key) result(hash_value)
      complex(dp), intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_cdp_2

   pure function hash64_cdp_3(key) result(hash_value)
      complex(dp), intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_cdp_3

   pure function hash64_logical_1(key) result(hash_value)
      logical, intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_logical_1

   pure function hash64_logical_2(key) result(hash_value)
      logical, intent(in) :: key(:, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_logical_2

   pure function hash64_logical_3(key) result(hash_value)
      logical, intent(in) :: key(:, :, :)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_logical_3

   pure function hash64_char_0(key) result(hash_value)
      character(len=*), intent(in) :: key
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_char_0

   pure function hash64_char_1(key) result(hash_value)
      character(len=*), intent(in) :: key(:)
      integer(int64) :: hash_value

      type(array_hash64_t) :: state

      call state%update(key)
      hash_value = state%digest()
   end function hash64_char_1

end module pic_array_hash
