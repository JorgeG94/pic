module test_pic_rng
   !! Tests for pic_rng.
   !!
   !! The golden vectors below are the published reference outputs of the two
   !! algorithms, converted to the signed decimal values Fortran can express:
   !!
   !!  * SplitMix64: the reference implementation is Sebastiano Vigna's
   !!    public-domain splitmix64.c (https://prng.di.unimi.it/splitmix64.c).
   !!    Seeding with 0 and drawing ten times gives
   !!    e220a8397b1dcdaf 6e789e6aa1b965f4 06c45d188009454f f88bb8a8724c81ec
   !!    1b39896a51a8749b 53cb9f0c747ea2ea 2c829abe1f4532e1 c584133ac916ab3c
   !!    3ee5789041c98ac3 f3b8488c368cb0a6
   !!    which are the values every SplitMix64 implementation is checked against.
   !!  * PCG32 (PCG-XSH-RR 64/32): Melissa O'Neill's pcg32-demo, seeded with
   !!    pcg32_srandom_r(&rng, 42u, 54u), prints
   !!    a15c02b7 7b47f409 ba1d3330 83d2f293 bfa4784b cbed606e bfc6a3ad
   !!    812fff6d e61f305a f9384b90
   !!    as its first ten 32-bit outputs.
   !!
   !! The hexadecimal words were converted to two's-complement signed decimals
   !! with an independent transcription of both algorithms in exact (arbitrary
   !! precision) arithmetic. These values are the regression anchor: if a
   !! compiler ever changes how the modular arithmetic here behaves, these tests
   !! fail immediately instead of silently perturbing someone's simulation.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, dp, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_rng, only: splitmix64_t, pcg32_t, next_real_dp, next_below, stream_for
   implicit none
   private
   public :: collect_pic_rng_tests

contains

   subroutine collect_pic_rng_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("splitmix64_golden_seed0", test_splitmix64_golden_seed0), &
                  new_unittest("splitmix64_golden_seed42", test_splitmix64_golden_seed42), &
                  new_unittest("splitmix64_reproducible", test_splitmix64_reproducible), &
                  new_unittest("splitmix64_next_u64", test_splitmix64_next_u64), &
                  new_unittest("pcg32_golden", test_pcg32_golden), &
                  new_unittest("pcg32_golden_unit_seed", test_pcg32_golden_unit_seed), &
                  new_unittest("pcg32_default_stream", test_pcg32_default_stream), &
                  new_unittest("pcg32_next_u64", test_pcg32_next_u64), &
                  new_unittest("pcg32_streams_differ", test_pcg32_streams_differ), &
                  new_unittest("real_construction", test_real_construction), &
                  new_unittest("real_range_splitmix64", test_real_range_splitmix64), &
                  new_unittest("real_range_pcg32", test_real_range_pcg32), &
                  new_unittest("below_coverage_splitmix64", test_below_coverage_splitmix64), &
                  new_unittest("below_coverage_pcg32", test_below_coverage_pcg32), &
                  new_unittest("below_one_is_zero", test_below_one_is_zero), &
                  new_unittest("below_invalid_n", test_below_invalid_n), &
                  new_unittest("below_invalid_n_no_error", test_below_invalid_n_no_error), &
                  new_unittest("below_large_bound", test_below_large_bound), &
                  new_unittest("below_power_of_two", test_below_power_of_two), &
                  new_unittest("stream_for_splitmix64", test_stream_for_splitmix64), &
                  new_unittest("stream_for_pcg32", test_stream_for_pcg32), &
                  new_unittest("stream_for_order_independent", test_stream_for_order_independent) &
                  ]
   end subroutine collect_pic_rng_tests

   subroutine golden_splitmix64_seed0(expected)
      integer(int64), intent(out) :: expected(10)
      expected = [-2152535657050944081_int64, &
                  7960286522194355700_int64, &
                  487617019471545679_int64, &
                  -537132696929009172_int64, &
                  1961750202426094747_int64, &
                  6038094601263162090_int64, &
                  3207296026000306913_int64, &
                  -4214222208109204676_int64, &
                  4532161160992623299_int64, &
                  -884877559730491226_int64]
   end subroutine golden_splitmix64_seed0

   subroutine test_splitmix64_golden_seed0(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(int64) :: expected(10)
      integer(default_int) :: i

      call golden_splitmix64_seed0(expected)
      call gen%seed(0_int64)
      do i = 1, 10
         call check(error, gen%next() == expected(i), "splitmix64 seed 0 golden mismatch")
         if (allocated(error)) return
      end do
   end subroutine test_splitmix64_golden_seed0

   subroutine test_splitmix64_golden_seed42(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(int64) :: expected(10)
      integer(default_int) :: i

      ! Same reference algorithm, seed 42.
      expected = [-4767286540954276203_int64, &
                  2949826092126892291_int64, &
                  5139283748462763858_int64, &
                  6349198060258255764_int64, &
                  701532786141963250_int64, &
                  -2430762948046562554_int64, &
                  4028864712777624925_int64, &
                  -3677692746721775708_int64, &
                  6270620877612482005_int64, &
                  -7037763681458882642_int64]

      call gen%seed(42_int64)
      do i = 1, 10
         call check(error, gen%next() == expected(i), "splitmix64 seed 42 golden mismatch")
         if (allocated(error)) return
      end do
   end subroutine test_splitmix64_golden_seed42

   subroutine test_splitmix64_reproducible(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: a, b
      integer(int64) :: va, vb
      integer(default_int) :: i

      ! A seed with the high bit set exercises the wraparound paths.
      call a%seed(-1234567890123456789_int64)
      call b%seed(-1234567890123456789_int64)
      do i = 1, 64
         va = a%next()
         vb = b%next()
         call check(error, va == vb, "splitmix64 is not reproducible")
         if (allocated(error)) return
      end do
   end subroutine test_splitmix64_reproducible

   subroutine test_splitmix64_next_u64(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(int64) :: expected(10)
      integer(default_int) :: i

      call golden_splitmix64_seed0(expected)
      call gen%seed(0_int64)
      do i = 1, 10
         call check(error, gen%next_u64() == expected(i), "splitmix64 next_u64 must equal next")
         if (allocated(error)) return
      end do
   end subroutine test_splitmix64_next_u64

   subroutine test_pcg32_golden(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: gen
      integer(int32) :: expected(10)
      integer(default_int) :: i

      expected = [-1587805513_int32, &
                  2068313097_int32, &
                  -1172491472_int32, &
                  -2083327341_int32, &
                  -1079740341_int32, &
                  -873635730_int32, &
                  -1077501011_int32, &
                  -2127560851_int32, &
                  -434163622_int32, &
                  -113751152_int32]

      call gen%seed(42_int64, 54_int64)
      do i = 1, 10
         call check(error, gen%next() == expected(i), "pcg32 42/54 golden mismatch")
         if (allocated(error)) return
      end do
   end subroutine test_pcg32_golden

   subroutine test_pcg32_golden_unit_seed(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: gen
      integer(int32) :: expected(10)
      integer(default_int) :: i

      ! Same reference algorithm, pcg32_srandom_r(&rng, 1u, 1u):
      ! c9828f91 1592e274 c0262657 a5c2b6d3 af811256
      ! 6c1c2879 30130b26 9cc21c33 d137ef42 908262ab
      expected = [-914190447_int32, &
                  361947764_int32, &
                  -1071241641_int32, &
                  -1513965869_int32, &
                  -1350495658_int32, &
                  1813784697_int32, &
                  806554406_int32, &
                  -1665000397_int32, &
                  -784863422_int32, &
                  -1870503253_int32]

      call gen%seed(1_int64, 1_int64)
      do i = 1, 10
         call check(error, gen%next() == expected(i), "pcg32 1/1 golden mismatch")
         if (allocated(error)) return
      end do
   end subroutine test_pcg32_golden_unit_seed

   subroutine test_pcg32_default_stream(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: a, b
      integer(default_int) :: i
      logical :: all_same

      ! An unseeded pcg32_t must still be a usable, deterministic generator:
      ! the default increment is odd and the default state is zero.
      all_same = .true.
      do i = 1, 32
         if (a%next() /= b%next()) all_same = .false.
      end do
      call check(error, all_same, "default-initialised pcg32 must be deterministic")
      if (allocated(error)) return
   end subroutine test_pcg32_default_stream

   subroutine test_pcg32_next_u64(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: gen
      integer(int64) :: word

      ! The first 32-bit draw supplies the high half: a15c02b7 7b47f409.
      call gen%seed(42_int64, 54_int64)
      word = gen%next_u64()
      call check(error, word == -6819572748675189751_int64, "pcg32 next_u64 high/low order wrong")
      if (allocated(error)) return

      word = gen%next_u64()
      call check(error, word == -5035812524867259757_int64, "pcg32 next_u64 second word wrong")
      if (allocated(error)) return
   end subroutine test_pcg32_next_u64

   subroutine test_pcg32_streams_differ(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: a, b
      integer(default_int) :: i
      logical :: any_differs

      call a%seed(42_int64, 1_int64)
      call b%seed(42_int64, 2_int64)
      any_differs = .false.
      do i = 1, 16
         if (a%next() /= b%next()) any_differs = .true.
      end do
      call check(error, any_differs, "pcg32 sequences must differ between increments")
      if (allocated(error)) return
   end subroutine test_pcg32_streams_differ

   subroutine test_real_construction(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      real(dp) :: value
      real(dp), parameter :: scale = 1.0_dp/9007199254740992.0_dp

      ! Both engines must return exactly (top 53 bits of the 64-bit word) * 2**-53.
      call sm%seed(0_int64)
      value = next_real_dp(sm)
      call check(error, abs(value - real(7956156453446585_int64, dp)*scale) <= 0.0_dp, &
                 "splitmix64 real is not the documented top-53-bit construction")
      if (allocated(error)) return

      call pcg%seed(42_int64, 54_int64)
      value = next_real_dp(pcg)
      call check(error, abs(value - real(5677329748551934_int64, dp)*scale) <= 0.0_dp, &
                 "pcg32 real is not the documented top-53-bit construction")
      if (allocated(error)) return
   end subroutine test_real_construction

   subroutine test_real_range_splitmix64(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      real(dp) :: value, smallest, largest
      integer(default_int) :: i

      call gen%seed(2024_int64)
      smallest = 1.0_dp
      largest = 0.0_dp
      do i = 1, 20000
         value = next_real_dp(gen)
         call check(error, value >= 0.0_dp .and. value < 1.0_dp, "splitmix64 real out of [0,1)")
         if (allocated(error)) return
         smallest = min(smallest, value)
         largest = max(largest, value)
      end do
      call check(error, smallest < 0.01_dp, "splitmix64 reals do not reach the low end")
      if (allocated(error)) return
      call check(error, largest > 0.99_dp, "splitmix64 reals do not reach the high end")
      if (allocated(error)) return
   end subroutine test_real_range_splitmix64

   subroutine test_real_range_pcg32(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: gen
      real(dp) :: value, smallest, largest
      integer(default_int) :: i

      call gen%seed(7_int64, 11_int64)
      smallest = 1.0_dp
      largest = 0.0_dp
      do i = 1, 20000
         value = next_real_dp(gen)
         call check(error, value >= 0.0_dp .and. value < 1.0_dp, "pcg32 real out of [0,1)")
         if (allocated(error)) return
         smallest = min(smallest, value)
         largest = max(largest, value)
      end do
      call check(error, smallest < 0.01_dp, "pcg32 reals do not reach the low end")
      if (allocated(error)) return
      call check(error, largest > 0.99_dp, "pcg32 reals do not reach the high end")
      if (allocated(error)) return
   end subroutine test_real_range_pcg32

   subroutine test_below_coverage_splitmix64(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: counts(0:5)
      integer(default_int) :: i, draw

      call gen%seed(99_int64)
      counts = 0_default_int
      do i = 1, 6000
         draw = next_below(gen, 6_default_int)
         call check(error, draw >= 0_default_int .and. draw < 6_default_int, &
                    "splitmix64 next_below left [0,n)")
         if (allocated(error)) return
         counts(draw) = counts(draw) + 1_default_int
      end do
      call check(error, all(counts > 0_default_int), "splitmix64 next_below misses residues")
      if (allocated(error)) return
   end subroutine test_below_coverage_splitmix64

   subroutine test_below_coverage_pcg32(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: gen
      integer(default_int) :: counts(0:6)
      integer(default_int) :: i, draw

      call gen%seed(5_int64, 3_int64)
      counts = 0_default_int
      do i = 1, 7000
         draw = next_below(gen, 7_default_int)
         call check(error, draw >= 0_default_int .and. draw < 7_default_int, &
                    "pcg32 next_below left [0,n)")
         if (allocated(error)) return
         counts(draw) = counts(draw) + 1_default_int
      end do
      call check(error, all(counts > 0_default_int), "pcg32 next_below misses residues")
      if (allocated(error)) return
   end subroutine test_below_coverage_pcg32

   subroutine test_below_one_is_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i

      call sm%seed(1_int64)
      call pcg%seed(1_int64, 1_int64)
      do i = 1, 32
         call check(error, next_below(sm, 1_default_int) == 0_default_int, &
                    "splitmix64 next_below(gen, 1) must be 0")
         if (allocated(error)) return
         call check(error, next_below(pcg, 1_default_int) == 0_default_int, &
                    "pcg32 next_below(gen, 1) must be 0")
         if (allocated(error)) return
      end do
   end subroutine test_below_one_is_zero

   subroutine test_below_invalid_n(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      type(error_t) :: err

      call sm%seed(3_int64)
      call pcg%seed(3_int64, 3_int64)

      call check(error, next_below(sm, 0_default_int, err) == 0_default_int, &
                 "splitmix64 next_below(gen, 0) must return 0")
      if (allocated(error)) return
      call check(error, err%has_error(), "splitmix64 next_below(gen, 0) must set an error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "expected ERROR_VALIDATION")
      if (allocated(error)) return

      call err%clear()
      call check(error, next_below(pcg, -5_default_int, err) == 0_default_int, &
                 "pcg32 next_below with negative n must return 0")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "expected ERROR_VALIDATION from pcg32")
      if (allocated(error)) return
   end subroutine test_below_invalid_n

   subroutine test_below_invalid_n_no_error(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg

      ! The optional error argument may be omitted; the call must still be safe.
      call sm%seed(3_int64)
      call pcg%seed(3_int64, 3_int64)
      call check(error, next_below(sm, 0_default_int) == 0_default_int, &
                 "splitmix64 next_below(gen, 0) without error must return 0")
      if (allocated(error)) return
      call check(error, next_below(pcg, 0_default_int) == 0_default_int, &
                 "pcg32 next_below(gen, 0) without error must return 0")
      if (allocated(error)) return
   end subroutine test_below_invalid_n_no_error

   subroutine test_below_large_bound(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i, draw
      integer(default_int) :: bound

      ! Largest representable bound: forces the mask loop to its final iteration
      ! and makes rejection actually possible.
      bound = huge(1_default_int)
      call sm%seed(17_int64)
      call pcg%seed(17_int64, 19_int64)
      do i = 1, 500
         draw = next_below(sm, bound)
         call check(error, draw >= 0_default_int .and. draw < bound, &
                    "splitmix64 next_below exceeded a huge bound")
         if (allocated(error)) return
         draw = next_below(pcg, bound)
         call check(error, draw >= 0_default_int .and. draw < bound, &
                    "pcg32 next_below exceeded a huge bound")
         if (allocated(error)) return
      end do
   end subroutine test_below_large_bound

   subroutine test_below_power_of_two(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: counts(0:255)
      integer(default_int) :: i, draw

      ! A power-of-two bound never rejects: every draw is accepted immediately.
      call gen%seed(31337_int64)
      counts = 0_default_int
      do i = 1, 20000
         draw = next_below(gen, 256_default_int)
         call check(error, draw >= 0_default_int .and. draw < 256_default_int, &
                    "next_below(gen, 256) left [0,256)")
         if (allocated(error)) return
         counts(draw) = counts(draw) + 1_default_int
      end do
      call check(error, all(counts > 0_default_int), "next_below(gen, 256) misses residues")
      if (allocated(error)) return
   end subroutine test_below_power_of_two

   subroutine test_stream_for_splitmix64(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: first, again, other
      integer(int64) :: a(8), b(8), c(8)
      integer(default_int) :: i

      call stream_for(20250101_int64, 7_default_int, first)
      call stream_for(20250101_int64, 7_default_int, again)
      call stream_for(20250101_int64, 8_default_int, other)
      do i = 1, 8
         a(i) = first%next()
         b(i) = again%next()
         c(i) = other%next()
      end do
      call check(error, all(a == b), "stream_for must be reproducible for the same id")
      if (allocated(error)) return
      call check(error, any(a /= c), "stream_for must differ between ids")
      if (allocated(error)) return
   end subroutine test_stream_for_splitmix64

   subroutine test_stream_for_pcg32(error)
      type(error_type), allocatable, intent(out) :: error
      type(pcg32_t) :: first, again, other, other_master
      integer(int32) :: a(8), b(8), c(8), d(8)
      integer(default_int) :: i

      call stream_for(20250101_int64, 3_default_int, first)
      call stream_for(20250101_int64, 3_default_int, again)
      call stream_for(20250101_int64, 4_default_int, other)
      call stream_for(-20250101_int64, 3_default_int, other_master)
      do i = 1, 8
         a(i) = first%next()
         b(i) = again%next()
         c(i) = other%next()
         d(i) = other_master%next()
      end do
      call check(error, all(a == b), "pcg32 stream_for must be reproducible for the same id")
      if (allocated(error)) return
      call check(error, any(a /= c), "pcg32 stream_for must differ between ids")
      if (allocated(error)) return
      call check(error, any(a /= d), "pcg32 stream_for must differ between master seeds")
      if (allocated(error)) return
   end subroutine test_stream_for_pcg32

   subroutine test_stream_for_order_independent(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(int64) :: forward(0:15), backward(0:15)
      integer(default_int) :: i

      ! Building the streams in the opposite order must not change any of them:
      ! this is what makes parallel results independent of scheduling.
      do i = 0, 15
         call stream_for(12345_int64, i, gen)
         forward(i) = gen%next()
      end do
      do i = 15, 0, -1
         call stream_for(12345_int64, i, gen)
         backward(i) = gen%next()
      end do
      call check(error, all(forward == backward), "stream_for depends on construction order")
      if (allocated(error)) return
      call check(error, count(forward == forward(0)) == 1, "stream seeds collide")
      if (allocated(error)) return
   end subroutine test_stream_for_order_independent

end module test_pic_rng
