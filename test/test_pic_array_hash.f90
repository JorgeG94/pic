module test_pic_array_hash
   !! Tests for pic_array_hash.
   !!
   !! Every hardcoded digest in this file was produced by an independent
   !! reference implementation written straight from the FNV-1a definition
   !! (offset basis 0x811C9DC5, prime 0x01000193, xor-then-multiply modulo
   !! 2**32) fed with the byte stream documented in the pic_array_hash header.
   !! None of them were obtained by running the code under test. The reference
   !! is cross-checked against the four FNV-1a values already asserted in
   !! test_pic_hash.f90 -- int8, int16, int32 and int64 [1,2,3,4,5] and the
   !! string "hello" -- which it reproduces exactly; test_agrees_with_fnv_1a
   !! below re-asserts that agreement from the Fortran side.
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int8, int16, int32, int64, sp, dp
   use pic_array_hash, only: array_hash, array_hash_t, array_hash_hex, &
                             ARRAY_HASH_OFFSET_BASIS
   use pic_hash_32bit_fnv, only: fnv_1a_hash, little_endian
   implicit none
   private

   public :: collect_pic_array_hash_tests

   ! Known answers, see the module comment above for provenance.
   integer(int32), parameter :: KAT_INT8_6 = 64115370_int32
   integer(int32), parameter :: KAT_INT8_12 = 1427995285_int32
   integer(int32), parameter :: KAT_INT16_6 = 361101554_int32
   integer(int32), parameter :: KAT_INT16_12 = 93406633_int32
   integer(int32), parameter :: KAT_INT32_6 = -1882140126_int32
   integer(int32), parameter :: KAT_INT32_12 = -1604064951_int32
   integer(int32), parameter :: KAT_INT64_6 = -800104830_int32
   integer(int32), parameter :: KAT_INT64_12 = 634141385_int32
   integer(int32), parameter :: KAT_RSP_6 = -807339842_int32
   integer(int32), parameter :: KAT_RSP_12 = -345658404_int32
   integer(int32), parameter :: KAT_RDP_6 = 217384566_int32
   integer(int32), parameter :: KAT_RDP_12 = -1840258436_int32
   integer(int32), parameter :: KAT_CSP_6 = 2136045186_int32
   integer(int32), parameter :: KAT_CSP_12 = 1806286438_int32
   integer(int32), parameter :: KAT_CDP_6 = -220600750_int32
   integer(int32), parameter :: KAT_CDP_12 = -1947427322_int32
   integer(int32), parameter :: KAT_LOG_6 = -1387572148_int32
   integer(int32), parameter :: KAT_LOG_12 = -603814595_int32
   integer(int32), parameter :: KAT_CHAR_HELLO = 1335831723_int32
   integer(int32), parameter :: KAT_CHAR_ARR3 = -12088790_int32
   integer(int32), parameter :: KAT_SCALARS = -1759583865_int32
   integer(int64), parameter :: KAT_SCALARS_BYTES = 101_int64
   integer(int32), parameter :: KAT_ZERO = 892911165_int32
   integer(int32), parameter :: KAT_NAN = -747944487_int32
   integer(int32), parameter :: KAT_POS_INF = -434144657_int32
   integer(int32), parameter :: KAT_NEG_INF = 229383254_int32
   integer(int32), parameter :: KAT_STREAM = 2028575337_int32
   integer(int64), parameter :: KAT_STREAM_BYTES = 43_int64
   integer(int32), parameter :: KAT_SEEDED = 106445289_int32
   integer(int32), parameter :: SEED = 12345_int32

contains

   subroutine collect_pic_array_hash_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("kat_integer_ranks", test_kat_integer_ranks), &
                  new_unittest("kat_real_ranks", test_kat_real_ranks), &
                  new_unittest("kat_complex_ranks", test_kat_complex_ranks), &
                  new_unittest("kat_logical_ranks", test_kat_logical_ranks), &
                  new_unittest("kat_character", test_kat_character), &
                  new_unittest("reshape_invariance", test_reshape_invariance), &
                  new_unittest("shape_sensitive_idiom", test_shape_sensitive_idiom), &
                  new_unittest("empty_arrays", test_empty_arrays), &
                  new_unittest("signed_zero", test_signed_zero), &
                  new_unittest("nan_canonicalisation", test_nan_canonicalisation), &
                  new_unittest("infinities", test_infinities), &
                  new_unittest("finite_values_distinct", test_finite_values_distinct), &
                  new_unittest("streaming_is_concatenation", test_streaming_is_concatenation), &
                  new_unittest("scalar_updates", test_scalar_updates), &
                  new_unittest("combine_sub_digests", test_combine_sub_digests), &
                  new_unittest("reset_and_seed", test_reset_and_seed), &
                  new_unittest("hex_formatting", test_hex_formatting), &
                  new_unittest("agrees_with_fnv_1a", test_agrees_with_fnv_1a) &
                  ]
   end subroutine collect_pic_array_hash_tests

   ! ---- shared test data ----------------------------------------------------

   subroutine make_reals(values)
      !! -1.5, -1.0, -0.5, 0.0, 0.5, ... all exactly representable in sp and dp.
      real(dp), intent(out) :: values(12)
      integer(default_int) :: i

      do i = 1, 12
         values(i) = real(i, dp)*0.5_dp - 2.0_dp
      end do
   end subroutine make_reals

   ! ---- known answers -------------------------------------------------------

   subroutine test_kat_integer_ranks(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: a8(12)
      integer(int16) :: a16(12)
      integer(int32) :: a32(12)
      integer(int64) :: a64(12)
      integer(int8) :: ext8(4)
      integer(int32) :: ext32(3)
      integer(default_int) :: i

      do i = 1, 12
         a8(i) = int(i, int8)
         a16(i) = int(i, int16)
         a32(i) = int(i, int32)
         a64(i) = int(i, int64)
      end do

      call check(error, array_hash(a8(1:6)) == KAT_INT8_6, "int8 rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a8(1:6), [2, 3])) == KAT_INT8_6, "int8 rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a8, [2, 3, 2])) == KAT_INT8_12, "int8 rank 3")
      if (allocated(error)) return

      call check(error, array_hash(a16(1:6)) == KAT_INT16_6, "int16 rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a16(1:6), [2, 3])) == KAT_INT16_6, "int16 rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a16, [2, 3, 2])) == KAT_INT16_12, "int16 rank 3")
      if (allocated(error)) return

      call check(error, array_hash(a32(1:6)) == KAT_INT32_6, "int32 rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a32(1:6), [2, 3])) == KAT_INT32_6, "int32 rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a32, [2, 3, 2])) == KAT_INT32_12, "int32 rank 3")
      if (allocated(error)) return

      call check(error, array_hash(a64(1:6)) == KAT_INT64_6, "int64 rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a64(1:6), [2, 3])) == KAT_INT64_6, "int64 rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(a64, [2, 3, 2])) == KAT_INT64_12, "int64 rank 3")
      if (allocated(error)) return

      ! negative and extreme integers must survive the two's-complement split.
      ! The most negative value of each kind is built at run time: written as a
      ! literal it trips gfortran's constant range check, since Fortran literals
      ! are symmetric about zero.
      ext8 = [-huge(1_int8), -1_int8, 0_int8, huge(1_int8)]
      ext8(1) = ext8(1) - 1_int8
      ext32 = [-1_int32, -huge(1_int32), huge(1_int32)]
      ext32(2) = ext32(2) - 1_int32

      call check(error, array_hash(ext8) == 2041571021_int32, "int8 extremes")
      if (allocated(error)) return
      call check(error, array_hash(ext32) == 212209005_int32, "int32 extremes")
   end subroutine test_kat_integer_ranks

   subroutine test_kat_real_ranks(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: d(12)
      real(sp) :: s(12)

      call make_reals(d)
      s = real(d, sp)

      call check(error, array_hash(s(1:6)) == KAT_RSP_6, "sp rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(s(1:6), [2, 3])) == KAT_RSP_6, "sp rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(s, [2, 3, 2])) == KAT_RSP_12, "sp rank 3")
      if (allocated(error)) return

      call check(error, array_hash(d(1:6)) == KAT_RDP_6, "dp rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d(1:6), [2, 3])) == KAT_RDP_6, "dp rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d, [2, 3, 2])) == KAT_RDP_12, "dp rank 3")
      if (allocated(error)) return

      ! same numeric values, different kind: documented to differ
      call check(error, array_hash(s(1:6)) /= array_hash(d(1:6)), "sp and dp must differ")
   end subroutine test_kat_real_ranks

   subroutine test_kat_complex_ranks(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: d(12)
      complex(dp) :: zd(12)
      complex(sp) :: zs(12)
      integer(default_int) :: i

      call make_reals(d)
      do i = 1, 12
         zd(i) = cmplx(d(i), -d(i), kind=dp)
         zs(i) = cmplx(real(d(i), sp), -real(d(i), sp), kind=sp)
      end do

      call check(error, array_hash(zs(1:6)) == KAT_CSP_6, "complex sp rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(zs(1:6), [2, 3])) == KAT_CSP_6, "complex sp rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(zs, [2, 3, 2])) == KAT_CSP_12, "complex sp rank 3")
      if (allocated(error)) return

      call check(error, array_hash(zd(1:6)) == KAT_CDP_6, "complex dp rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(zd(1:6), [2, 3])) == KAT_CDP_6, "complex dp rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(zd, [2, 3, 2])) == KAT_CDP_12, "complex dp rank 3")
   end subroutine test_kat_complex_ranks

   subroutine test_kat_logical_ranks(error)
      type(error_type), allocatable, intent(out) :: error
      logical :: l(12)
      integer(default_int) :: i

      do i = 1, 12
         l(i) = (mod(i, 2_default_int) == 1_default_int)
      end do

      call check(error, array_hash(l(1:6)) == KAT_LOG_6, "logical rank 1")
      if (allocated(error)) return
      call check(error, array_hash(reshape(l(1:6), [2, 3])) == KAT_LOG_6, "logical rank 2")
      if (allocated(error)) return
      call check(error, array_hash(reshape(l, [2, 3, 2])) == KAT_LOG_12, "logical rank 3")
      if (allocated(error)) return
      call check(error, array_hash([.true.]) /= array_hash([.false.]), "true must differ from false")
   end subroutine test_kat_logical_ranks

   subroutine test_kat_character(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=2) :: words(3)

      words = ["ab", "cd", "ef"]

      call check(error, array_hash("hello") == KAT_CHAR_HELLO, "scalar string")
      if (allocated(error)) return
      call check(error, array_hash(words) == KAT_CHAR_ARR3, "string array")
      if (allocated(error)) return
      ! an array of strings is the concatenation of its elements
      call check(error, array_hash(words) == array_hash("abcdef"), "strings concatenate")
   end subroutine test_kat_character

   ! ---- shape contract ------------------------------------------------------

   subroutine test_reshape_invariance(error)
      !! The documented contract: the stream carries values, never the shape.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: d(12)
      integer(int32) :: flat_hash

      call make_reals(d)
      flat_hash = array_hash(d)

      call check(error, array_hash(reshape(d, [2, 6])) == flat_hash, "2x6 matches flat")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d, [6, 2])) == flat_hash, "6x2 matches flat")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d, [3, 4])) == flat_hash, "3x4 matches flat")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d, [2, 2, 3])) == flat_hash, "2x2x3 matches flat")
      if (allocated(error)) return
      call check(error, array_hash(reshape(d, [1, 12])) == flat_hash, "1x12 matches flat")
   end subroutine test_reshape_invariance

   subroutine test_shape_sensitive_idiom(error)
      !! The documented way to opt into shape sensitivity.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: d(12)
      type(array_hash_t) :: wide, tall
      integer(int32) :: h_wide, h_tall

      call make_reals(d)

      call wide%update(shape(reshape(d, [2, 6]), kind=int32))
      call wide%update(reshape(d, [2, 6]))
      h_wide = wide%digest()

      call tall%update(shape(reshape(d, [6, 2]), kind=int32))
      call tall%update(reshape(d, [6, 2]))
      h_tall = tall%digest()

      call check(error, h_wide /= h_tall, "hashing the shape first must separate 2x6 from 6x2")
      if (allocated(error)) return
      call check(error, wide%bytes_hashed() == 8_int64 + 12_int64*14_int64, "shape plus data bytes")
   end subroutine test_shape_sensitive_idiom

   ! ---- degenerate input ----------------------------------------------------

   subroutine test_empty_arrays(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: e8(0)
      integer(int16) :: e16(0)
      integer(int32) :: e32(0, 3)
      integer(int64) :: e64(0, 2, 2)
      real(sp) :: esp(0)
      real(dp) :: edp(2, 0)
      complex(sp) :: ecsp(0)
      complex(dp) :: ecdp(1, 1, 0)
      logical :: el(0)
      character(len=4) :: estr(0)
      type(array_hash_t) :: state

      call check(error, array_hash(e8) == ARRAY_HASH_OFFSET_BASIS, "empty int8")
      if (allocated(error)) return
      call check(error, array_hash(e16) == ARRAY_HASH_OFFSET_BASIS, "empty int16")
      if (allocated(error)) return
      call check(error, array_hash(e32) == ARRAY_HASH_OFFSET_BASIS, "empty int32 rank 2")
      if (allocated(error)) return
      call check(error, array_hash(e64) == ARRAY_HASH_OFFSET_BASIS, "empty int64 rank 3")
      if (allocated(error)) return
      call check(error, array_hash(esp) == ARRAY_HASH_OFFSET_BASIS, "empty sp")
      if (allocated(error)) return
      call check(error, array_hash(edp) == ARRAY_HASH_OFFSET_BASIS, "empty dp rank 2")
      if (allocated(error)) return
      call check(error, array_hash(ecsp) == ARRAY_HASH_OFFSET_BASIS, "empty complex sp")
      if (allocated(error)) return
      call check(error, array_hash(ecdp) == ARRAY_HASH_OFFSET_BASIS, "empty complex dp rank 3")
      if (allocated(error)) return
      call check(error, array_hash(el) == ARRAY_HASH_OFFSET_BASIS, "empty logical")
      if (allocated(error)) return
      call check(error, array_hash(estr) == ARRAY_HASH_OFFSET_BASIS, "empty string array")
      if (allocated(error)) return
      call check(error, array_hash("") == ARRAY_HASH_OFFSET_BASIS, "zero length string")
      if (allocated(error)) return

      ! update on an empty array is a no-op, not an error
      call state%update(e8)
      call state%update(edp)
      call state%update(estr)
      call check(error, state%digest() == ARRAY_HASH_OFFSET_BASIS, "empty updates are no-ops")
      if (allocated(error)) return
      call check(error, state%bytes_hashed() == 0_int64, "empty updates consume no bytes")
   end subroutine test_empty_arrays

   ! ---- floating point policy ----------------------------------------------

   subroutine test_signed_zero(error)
      !! -0.0 and +0.0 compare equal, so they must hash equal.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: pos_d, neg_d
      real(sp) :: pos_s, neg_s

      pos_d = 0.0_dp
      neg_d = sign(0.0_dp, -1.0_dp)
      pos_s = 0.0_sp
      neg_s = sign(0.0_sp, -1.0_sp)

      call check(error, array_hash([pos_d]) == KAT_ZERO, "dp +0.0 canonical record")
      if (allocated(error)) return
      call check(error, array_hash([neg_d]) == KAT_ZERO, "dp -0.0 canonical record")
      if (allocated(error)) return
      call check(error, array_hash([pos_s]) == KAT_ZERO, "sp +0.0 canonical record")
      if (allocated(error)) return
      call check(error, array_hash([neg_s]) == KAT_ZERO, "sp -0.0 canonical record")
      if (allocated(error)) return
      call check(error, array_hash([pos_d, neg_d]) == array_hash([neg_d, pos_d]), &
                 "mixed signed zeros are indistinguishable")
      if (allocated(error)) return
      call check(error, array_hash([pos_d]) /= array_hash([1.0_dp]), "zero differs from one")
   end subroutine test_signed_zero

   subroutine test_nan_canonicalisation(error)
      !! Every NaN collapses to one record, whatever its sign or payload.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: nan_d, neg_nan_d
      real(sp) :: nan_s, neg_nan_s

      nan_d = ieee_value(1.0_dp, ieee_quiet_nan)
      neg_nan_d = -nan_d
      nan_s = ieee_value(1.0_sp, ieee_quiet_nan)
      neg_nan_s = -nan_s

      call check(error, array_hash([nan_d]) == KAT_NAN, "dp NaN canonical record")
      if (allocated(error)) return
      call check(error, array_hash([neg_nan_d]) == KAT_NAN, "dp negative NaN canonical record")
      if (allocated(error)) return
      call check(error, array_hash([nan_s]) == KAT_NAN, "sp NaN canonical record")
      if (allocated(error)) return
      call check(error, array_hash([neg_nan_s]) == KAT_NAN, "sp negative NaN canonical record")
      if (allocated(error)) return
      ! a NaN never equals itself numerically but does hash equal to itself
      call check(error, array_hash([nan_d, nan_d]) == array_hash([neg_nan_d, nan_d]), &
                 "NaN payloads are not observable")
      if (allocated(error)) return
      call check(error, array_hash([nan_d]) /= array_hash([0.0_dp]), "NaN differs from zero")
      if (allocated(error)) return
      call check(error, array_hash([nan_d]) /= array_hash([1.0_dp]), "NaN differs from one")
   end subroutine test_nan_canonicalisation

   subroutine test_infinities(error)
      !! +Inf and -Inf stay distinct from each other and from everything else.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: pinf_d, ninf_d, nan_d
      real(sp) :: pinf_s, ninf_s

      pinf_d = ieee_value(1.0_dp, ieee_positive_inf)
      ninf_d = ieee_value(1.0_dp, ieee_negative_inf)
      nan_d = ieee_value(1.0_dp, ieee_quiet_nan)
      pinf_s = ieee_value(1.0_sp, ieee_positive_inf)
      ninf_s = ieee_value(1.0_sp, ieee_negative_inf)

      call check(error, array_hash([pinf_d]) == KAT_POS_INF, "dp +Inf canonical record")
      if (allocated(error)) return
      call check(error, array_hash([ninf_d]) == KAT_NEG_INF, "dp -Inf canonical record")
      if (allocated(error)) return
      call check(error, array_hash([pinf_s]) == KAT_POS_INF, "sp +Inf canonical record")
      if (allocated(error)) return
      call check(error, array_hash([ninf_s]) == KAT_NEG_INF, "sp -Inf canonical record")
      if (allocated(error)) return
      call check(error, array_hash([pinf_d]) /= array_hash([ninf_d]), "+Inf differs from -Inf")
      if (allocated(error)) return
      call check(error, array_hash([pinf_d]) /= array_hash([nan_d]), "+Inf differs from NaN")
      if (allocated(error)) return
      call check(error, array_hash([pinf_d]) /= array_hash([huge(1.0_dp)]), &
                 "+Inf differs from huge")
      if (allocated(error)) return
      call check(error, array_hash([ninf_d]) /= array_hash([-huge(1.0_dp)]), &
                 "-Inf differs from -huge")
      if (allocated(error)) return
      ! infinities inside a complex value are classified the same way
      call check(error, array_hash([cmplx(pinf_d, ninf_d, kind=dp)]) &
                 /= array_hash([cmplx(ninf_d, pinf_d, kind=dp)]), "complex infinities ordered")
   end subroutine test_infinities

   subroutine test_finite_values_distinct(error)
      !! The finite decomposition is injective, including across the subnormal
      !! range and at the extremes of the exponent.
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: values(8)
      integer(default_int) :: i, j

      values = [1.0_dp, -1.0_dp, 2.0_dp, 0.5_dp, tiny(1.0_dp), &
                scale(tiny(1.0_dp), -1), huge(1.0_dp), nearest(1.0_dp, 2.0_dp)]

      do i = 1, 8
         do j = i + 1, 8
            call check(error, array_hash([values(i)]) /= array_hash([values(j)]), &
                       "distinct finite values must hash differently")
            if (allocated(error)) return
         end do
      end do

      call check(error, array_hash([1.0_dp]) /= array_hash([-1.0_dp]), "sign is encoded")
   end subroutine test_finite_values_distinct

   ! ---- streaming -----------------------------------------------------------

   subroutine test_streaming_is_concatenation(error)
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: state
      integer(int32) :: counts(3)
      real(dp) :: values(2)

      counts = [1_int32, 2_int32, 3_int32]
      values = [1.5_dp, -0.25_dp]

      call state%update(counts)
      call state%update(values)
      call state%update("tag")

      call check(error, state%digest() == KAT_STREAM, "streamed digest")
      if (allocated(error)) return
      call check(error, state%bytes_hashed() == KAT_STREAM_BYTES, "streamed byte count")
      if (allocated(error)) return

      ! streaming two arrays of one type equals hashing their concatenation
      call check(error, stream_two_halves() == array_hash([1_int32, 2_int32, 3_int32, &
                                                           4_int32, 5_int32, 6_int32]), &
                 "streaming equals concatenation")
      if (allocated(error)) return
      ! order matters
      call check(error, array_hash([1_int32, 2_int32]) /= array_hash([2_int32, 1_int32]), &
                 "order is significant")
   end subroutine test_streaming_is_concatenation

   function stream_two_halves() result(hash_value)
      integer(int32) :: hash_value
      type(array_hash_t) :: state

      call state%update([1_int32, 2_int32, 3_int32])
      call state%update([4_int32, 5_int32, 6_int32])
      hash_value = state%digest()
   end function stream_two_halves

   subroutine test_scalar_updates(error)
      !! The rank-0 update overloads, which is how a derived type's scalar
      !! components join the same digest as its arrays.
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: state

      call state%update(-7_int8)
      call state%update(-7_int16)
      call state%update(-7_int32)
      call state%update(-7_int64)
      call state%update(-2.5_sp)
      call state%update(-2.5_dp)
      call state%update(cmplx(1.5_sp, -0.5_sp, kind=sp))
      call state%update(cmplx(1.5_dp, -0.5_dp, kind=dp))
      call state%update(.true.)
      call state%update("z")

      call check(error, state%digest() == KAT_SCALARS, "scalar stream digest")
      if (allocated(error)) return
      call check(error, state%bytes_hashed() == KAT_SCALARS_BYTES, "scalar stream byte count")
      if (allocated(error)) return
      ! a scalar and a one-element array are the same stream
      call check(error, scalar_only() == array_hash([-7_int32]), "scalar equals 1-element array")
   end subroutine test_scalar_updates

   function scalar_only() result(hash_value)
      integer(int32) :: hash_value
      type(array_hash_t) :: state

      call state%update(-7_int32)
      hash_value = state%digest()
   end function scalar_only

   subroutine test_combine_sub_digests(error)
      !! Sub-digests fold in as ordinary int32 data, and the fold is ordered.
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: forwards, backwards
      integer(int32) :: part_a, part_b

      part_a = array_hash([1_int32, 2_int32, 3_int32])
      part_b = array_hash([4.0_dp, 5.0_dp])

      call forwards%update([part_a, part_b])
      call backwards%update([part_b, part_a])

      call check(error, forwards%digest() /= backwards%digest(), "combining is ordered")
      if (allocated(error)) return
      call check(error, forwards%bytes_hashed() == 8_int64, "two digests are eight bytes")
      if (allocated(error)) return
      call check(error, forwards%digest() == array_hash([part_a, part_b]), &
                 "combining is just hashing the digests")
   end subroutine test_combine_sub_digests

   subroutine test_reset_and_seed(error)
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: state

      call state%update([1.0_dp, 2.0_dp, 3.0_dp])
      call check(error, state%bytes_hashed() == 42_int64, "three dp reals are 42 bytes")
      if (allocated(error)) return

      call state%reset()
      call check(error, state%digest() == ARRAY_HASH_OFFSET_BASIS, "reset returns to the basis")
      if (allocated(error)) return
      call check(error, state%bytes_hashed() == 0_int64, "reset clears the byte count")
      if (allocated(error)) return

      call state%update([1_int32, 2_int32, 3_int32])
      call check(error, state%digest() == array_hash([1_int32, 2_int32, 3_int32]), &
                 "a reset accumulator behaves like a fresh one")
      if (allocated(error)) return

      call state%reset(SEED)
      call state%update([1_int32, 2_int32, 3_int32])
      call check(error, state%digest() == KAT_SEEDED, "seeded digest")
      if (allocated(error)) return
      call check(error, state%digest() /= array_hash([1_int32, 2_int32, 3_int32]), &
                 "a seed separates the domain")
   end subroutine test_reset_and_seed

   subroutine test_hex_formatting(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, array_hash_hex(ARRAY_HASH_OFFSET_BASIS) == "811c9dc5", "basis in hex")
      if (allocated(error)) return
      call check(error, array_hash_hex(KAT_INT32_6) == "8fd0d222", "negative digest in hex")
      if (allocated(error)) return
      ! the remaining nibbles, so the whole lookup table is exercised
      call check(error, array_hash_hex(879209440_int32) == "3467abe0", "positive digest in hex")
      if (allocated(error)) return
      call check(error, array_hash_hex(0_int32) == "00000000", "zero in hex")
      if (allocated(error)) return
      call check(error, len(array_hash_hex(KAT_INT32_6)) == 8, "hex is always eight characters")
   end subroutine test_hex_formatting

   subroutine test_agrees_with_fnv_1a(error)
      !! pic_array_hash must reproduce pic_hash_32bit_fnv on the data both can
      !! hash, which is what makes the new module an extension rather than a
      !! second, incompatible hash. The multi-byte integer cases only coincide
      !! on a little-endian host: fnv_1a_hash walks storage order, while
      !! pic_array_hash always emits least-significant-byte-first.
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: a8(5)
      integer(int16) :: a16(5)
      integer(int32) :: a32(5)
      integer(int64) :: a64(5)
      integer(default_int) :: i

      do i = 1, 5
         a8(i) = int(i, int8)
         a16(i) = int(i, int16)
         a32(i) = int(i, int32)
         a64(i) = int(i, int64)
      end do

      call check(error, array_hash(a8) == fnv_1a_hash(a8), "int8 matches fnv_1a_hash")
      if (allocated(error)) return
      call check(error, array_hash("hello") == fnv_1a_hash("hello"), "string matches fnv_1a_hash")
      if (allocated(error)) return

      if (little_endian) then
         call check(error, array_hash(a16) == fnv_1a_hash(a16), "int16 matches fnv_1a_hash")
         if (allocated(error)) return
         call check(error, array_hash(a32) == fnv_1a_hash(a32), "int32 matches fnv_1a_hash")
         if (allocated(error)) return
         call check(error, array_hash(a64) == fnv_1a_hash(a64), "int64 matches fnv_1a_hash")
         if (allocated(error)) return
      end if

      ! and the values test_pic_hash.f90 already pins down
      call check(error, array_hash(a8) == -1075497752_int32, "int8 known answer")
   end subroutine test_agrees_with_fnv_1a

end module test_pic_array_hash
