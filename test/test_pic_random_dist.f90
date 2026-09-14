module test_pic_random_dist
   !! Tests for pic_random_dist and pic_random_dist_real.
   !!
   !! The pinned sequences below are the cross-compiler contract for the
   !! integer tier. They were produced by an independent Python reference that
   !! implements the documented sampling procedure -- table lookup, twelve bits
   !! for the cell, thirty-one to interpolate, memoryless tail -- on top of
   !! Python mirrors of the two generators. Neither was obtained by running the
   !! code under test.
   !!
   !! Those generator mirrors are themselves checked before being trusted: the
   !! SplitMix64 one against the five published outputs of `splitmix64.c` for
   !! seed 0, and the PCG32 one against the two `next_u64` values that
   !! test_pic_rng.f90 already asserts.
   !!
   !! If a compiler ever disagrees with one of these arrays, that compiler is
   !! not producing the same simulation as the others, which is exactly what
   !! the integer tier exists to prevent.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, dp
   use pic_error, only: error_t
   use pic_rng, only: splitmix64_t, pcg32_t
   use pic_random_dist, only: next_range, next_bernoulli_ppm, &
                              next_exponential_int, next_poisson_int, &
                              PPM_ONE, EXPONENTIAL_MAX_MEAN, POISSON_MAX_MEAN_MILLI
   use pic_random_dist_real, only: next_exponential_dp, next_normal_dp
   implicit none
   private

   public :: collect_pic_random_dist_tests

   integer(int64), parameter :: SEED = 20250914_int64
      !! Seed for the SplitMix64 pins.
   integer(int64), parameter :: PCG_STATE = 20250914_int64
      !! State for the PCG32 pins.
   integer(int64), parameter :: PCG_SEQ = 7_int64
      !! Stream selector for the PCG32 pins.

   integer(default_int), parameter :: BULK = 1000000_default_int
      !! Draws in the distribution-shape tests.

   real(dp), parameter :: EXPECTED_MEAN_AT_2 = 1.9793233_dp
      !! `exp(-1/4) / (1 - exp(-1/2))`, the mean of `round(2*X)` for
      !! `X ~ Exp(1)`. Computed independently, not measured from this code.
      !! Draws in the distribution-shape tests.

   integer(default_int), parameter :: RANGE_SM(32) = [ &
           1_default_int, 3_default_int, 3_default_int, 5_default_int, 2_default_int, 1_default_int, 4_default_int, 2_default_int, &
           5_default_int, 2_default_int, 1_default_int, 4_default_int, 6_default_int, 3_default_int, 3_default_int, 1_default_int, &
           5_default_int, 3_default_int, 3_default_int, 6_default_int, 2_default_int, 2_default_int, 2_default_int, 3_default_int, &
             4_default_int, 2_default_int, 2_default_int, 5_default_int, 1_default_int, 5_default_int, 2_default_int, 4_default_int]
   logical, parameter :: BERN_SM(32) = [ &
                         .true., .false., .true., .false., .false., .false., &
                         .false., .true., .false., .true., .false., .false., &
                         .false., .false., .false., .false., .true., .false., &
                         .false., .false., .false., .false., .false., .false., &
                         .true., .false., .false., .false., .false., .false., &
                         .true., .false.]
   integer(int64), parameter :: EXPO_SM(32) = [ &
                                167_int64, 689_int64, 389_int64, 240_int64, 1938_int64, 327_int64, &
                                388_int64, 115_int64, 353_int64, 94_int64, 165_int64, 180_int64, &
                                105_int64, 1693_int64, 2124_int64, 798_int64, 1115_int64, 1253_int64, &
                                1613_int64, 72_int64, 564_int64, 401_int64, 851_int64, 823_int64, &
                                3057_int64, 518_int64, 5_int64, 883_int64, 1370_int64, 2352_int64, &
                                1143_int64, 1951_int64]
   integer(int32), parameter :: POIS_SM(32) = [ &
                                5_int32, 8_int32, 3_int32, 5_int32, 4_int32, 4_int32, 2_int32, 6_int32, &
                                3_int32, 5_int32, 5_int32, 3_int32, 2_int32, 4_int32, 6_int32, 6_int32, &
                                3_int32, 3_int32, 3_int32, 5_int32, 2_int32, 1_int32, 9_int32, 3_int32, &
                                5_int32, 5_int32, 6_int32, 1_int32, 2_int32, 2_int32, 2_int32, 4_int32]
   integer(default_int), parameter :: RANGE_PCG(32) = [ &
           2_default_int, 3_default_int, 1_default_int, 2_default_int, 6_default_int, 1_default_int, 1_default_int, 6_default_int, &
           3_default_int, 3_default_int, 1_default_int, 6_default_int, 5_default_int, 1_default_int, 2_default_int, 6_default_int, &
           1_default_int, 4_default_int, 2_default_int, 6_default_int, 1_default_int, 3_default_int, 3_default_int, 3_default_int, &
             2_default_int, 6_default_int, 4_default_int, 5_default_int, 3_default_int, 6_default_int, 2_default_int, 5_default_int]
   logical, parameter :: BERN_PCG(32) = [ &
                         .true., .false., .false., .true., .false., .false., &
                         .false., .false., .false., .false., .false., .false., &
                         .false., .false., .false., .false., .false., .false., &
                         .false., .false., .false., .false., .true., .true., &
                         .false., .true., .false., .false., .false., .false., &
                         .true., .false.]
   integer(int64), parameter :: EXPO_PCG(32) = [ &
                                729_int64, 1452_int64, 1709_int64, 955_int64, 3756_int64, 1284_int64, &
                                1252_int64, 488_int64, 1468_int64, 2180_int64, 636_int64, 264_int64, &
                                114_int64, 117_int64, 570_int64, 369_int64, 2156_int64, 180_int64, &
                                217_int64, 558_int64, 330_int64, 2111_int64, 1403_int64, 482_int64, &
                                1468_int64, 161_int64, 2316_int64, 347_int64, 1112_int64, 1468_int64, &
                                40_int64, 1541_int64]
   integer(int32), parameter :: POIS_PCG(32) = [ &
                                2_int32, 1_int32, 3_int32, 5_int32, 5_int32, 1_int32, 3_int32, 4_int32, &
                                4_int32, 6_int32, 7_int32, 3_int32, 5_int32, 4_int32, 4_int32, 2_int32, &
                                3_int32, 3_int32, 6_int32, 4_int32, 5_int32, 1_int32, 2_int32, 3_int32, &
                                2_int32, 5_int32, 5_int32, 5_int32, 3_int32, 5_int32, 1_int32, 2_int32]

contains

   subroutine collect_pic_random_dist_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("pinned_range", test_pinned_range), &
                  new_unittest("pinned_bernoulli", test_pinned_bernoulli), &
                  new_unittest("pinned_exponential", test_pinned_exponential), &
                  new_unittest("pinned_poisson", test_pinned_poisson), &
                  new_unittest("range_edges", test_range_edges), &
                  new_unittest("range_full_width", test_range_full_width), &
                  new_unittest("range_is_uniform", test_range_is_uniform), &
                  new_unittest("bernoulli_edges", test_bernoulli_edges), &
                  new_unittest("bernoulli_rate", test_bernoulli_rate), &
                  new_unittest("exponential_edges", test_exponential_edges), &
                  new_unittest("exponential_shape", test_exponential_shape), &
                  new_unittest("exponential_small_mean", test_exponential_small_mean), &
                  new_unittest("poisson_edges", test_poisson_edges), &
                  new_unittest("poisson_shape", test_poisson_shape), &
                  new_unittest("real_tier_shape", test_real_tier_shape) &
                  ]
   end subroutine collect_pic_random_dist_tests

   ! ---- the cross-compiler pins --------------------------------------------

   subroutine test_pinned_range(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i

      call sm%seed(SEED)
      do i = 1, 32
         call check(error, next_range(sm, 1_default_int, 6_default_int) == RANGE_SM(i), &
                    "splitmix64 next_range diverged from the pinned sequence")
         if (allocated(error)) return
      end do

      call pcg%seed(PCG_STATE, PCG_SEQ)
      do i = 1, 32
         call check(error, next_range(pcg, 1_default_int, 6_default_int) == RANGE_PCG(i), &
                    "pcg32 next_range diverged from the pinned sequence")
         if (allocated(error)) return
      end do
   end subroutine test_pinned_range

   subroutine test_pinned_bernoulli(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i

      call sm%seed(SEED)
      do i = 1, 32
         call check(error, next_bernoulli_ppm(sm, 250000_default_int) .eqv. BERN_SM(i), &
                    "splitmix64 next_bernoulli_ppm diverged from the pinned sequence")
         if (allocated(error)) return
      end do

      call pcg%seed(PCG_STATE, PCG_SEQ)
      do i = 1, 32
         call check(error, next_bernoulli_ppm(pcg, 250000_default_int) .eqv. BERN_PCG(i), &
                    "pcg32 next_bernoulli_ppm diverged from the pinned sequence")
         if (allocated(error)) return
      end do
   end subroutine test_pinned_bernoulli

   subroutine test_pinned_exponential(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i

      call sm%seed(SEED)
      do i = 1, 32
         call check(error, next_exponential_int(sm, 1000_int64) == EXPO_SM(i), &
                    "splitmix64 next_exponential_int diverged from the pinned sequence")
         if (allocated(error)) return
      end do

      call pcg%seed(PCG_STATE, PCG_SEQ)
      do i = 1, 32
         call check(error, next_exponential_int(pcg, 1000_int64) == EXPO_PCG(i), &
                    "pcg32 next_exponential_int diverged from the pinned sequence")
         if (allocated(error)) return
      end do
   end subroutine test_pinned_exponential

   subroutine test_pinned_poisson(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: sm
      type(pcg32_t) :: pcg
      integer(default_int) :: i

      call sm%seed(SEED)
      do i = 1, 32
         call check(error, next_poisson_int(sm, 3500_default_int) == POIS_SM(i), &
                    "splitmix64 next_poisson_int diverged from the pinned sequence")
         if (allocated(error)) return
      end do

      call pcg%seed(PCG_STATE, PCG_SEQ)
      do i = 1, 32
         call check(error, next_poisson_int(pcg, 3500_default_int) == POIS_PCG(i), &
                    "pcg32 next_poisson_int diverged from the pinned sequence")
         if (allocated(error)) return
      end do
   end subroutine test_pinned_poisson

   ! ---- edges ---------------------------------------------------------------

   subroutine test_range_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      type(error_t) :: err
      integer(default_int) :: r, i

      call gen%seed(SEED)

      r = next_range(gen, 5_default_int, 5_default_int, err)
      call check(error, r == 5_default_int, "lo == hi returns lo")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), "lo == hi is not an error")
      if (allocated(error)) return

      r = next_range(gen, 7_default_int, 3_default_int, err)
      call check(error, r == 7_default_int, "hi < lo returns lo")
      if (allocated(error)) return
      call check(error, err%has_error(), "hi < lo sets an error")
      if (allocated(error)) return

      ! hi < lo without err must not crash either
      r = next_range(gen, 7_default_int, 3_default_int)
      call check(error, r == 7_default_int, "hi < lo without err still returns lo")
      if (allocated(error)) return

      ! negative ranges
      do i = 1, 200
         r = next_range(gen, -10_default_int, -5_default_int)
         call check(error, r >= -10_default_int .and. r <= -5_default_int, &
                    "a wholly negative range stays in bounds")
         if (allocated(error)) return
      end do
   end subroutine test_range_edges

   subroutine test_range_full_width(error)
      !! The whole of default_int. The span is one more than the type can
      !! hold, which is why it is computed as unsigned.
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: r, i, negatives
      integer(default_int) :: lo, hi

      lo = -huge(1_default_int) - 1_default_int
      hi = huge(1_default_int)

      call gen%seed(SEED)
      negatives = 0
      do i = 1, 20000
         r = next_range(gen, lo, hi)
         if (r < 0_default_int) negatives = negatives + 1
      end do

      ! exactly half the range is negative; 20000 draws put 5 sigma at ~354
      call check(error, abs(negatives - 10000) < 400, &
                 "the full range is not split evenly about zero")
      if (allocated(error)) return

      ! and the extremes are reachable at all: a mask that was one bit short
      ! would never produce the top of the range
      call gen%seed(SEED)
      r = 0_default_int
      do i = 1, 2000
         if (next_range(gen, hi - 3_default_int, hi) == hi) r = 1_default_int
      end do
      call check(error, r == 1_default_int, "the top of the range is never drawn")
   end subroutine test_range_full_width

   subroutine test_range_is_uniform(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: counts(6), i, r

      call gen%seed(SEED)
      counts = 0_default_int
      do i = 1, BULK
         r = next_range(gen, 1_default_int, 6_default_int)
         counts(r) = counts(r) + 1_default_int
      end do

      ! expected BULK/6 per face; sigma is sqrt(n p (1-p)) ~ 373, so 5 sigma
      ! is under 1900
      do i = 1, 6
         call check(error, abs(counts(i) - BULK/6_default_int) < 1900_default_int, &
                    "a face of the die is more than 5 sigma from uniform")
         if (allocated(error)) return
      end do
      call check(error, sum(counts) == BULK, "every draw landed in range")
   end subroutine test_range_is_uniform

   subroutine test_bernoulli_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: i

      call gen%seed(SEED)
      do i = 1, 100
         call check(error,.not. next_bernoulli_ppm(gen, 0_default_int), &
                    "zero ppm must never fire")
         if (allocated(error)) return
         call check(error, next_bernoulli_ppm(gen, PPM_ONE), &
                    "a million ppm must always fire")
         if (allocated(error)) return
         ! out of range clamps rather than erroring
         call check(error,.not. next_bernoulli_ppm(gen, -5000_default_int), &
                    "negative ppm clamps to never")
         if (allocated(error)) return
         call check(error, next_bernoulli_ppm(gen, PPM_ONE + 1000_default_int), &
                    "ppm above a million clamps to always")
         if (allocated(error)) return
      end do
   end subroutine test_bernoulli_edges

   subroutine test_bernoulli_rate(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: i, hits
      real(dp) :: rate, sigma

      call gen%seed(SEED)
      hits = 0
      do i = 1, BULK
         if (next_bernoulli_ppm(gen, 2500_default_int)) hits = hits + 1
      end do

      rate = real(hits, dp)/real(BULK, dp)
      sigma = sqrt(0.0025_dp*0.9975_dp/real(BULK, dp))
      call check(error, abs(rate - 0.0025_dp) < 5.0_dp*sigma, &
                 "the Bernoulli rate is more than 5 sigma from 2500 ppm")
   end subroutine test_bernoulli_rate

   subroutine test_exponential_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      type(error_t) :: err
      integer(int64) :: r

      call gen%seed(SEED)

      r = next_exponential_int(gen, 0_int64, err)
      call check(error, r == 0_int64, "a mean of zero draws zero")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), "a mean of zero is not an error")
      if (allocated(error)) return

      r = next_exponential_int(gen, -1_int64, err)
      call check(error, r == 0_int64, "a negative mean returns zero")
      if (allocated(error)) return
      call check(error, err%has_error(), "a negative mean sets an error")
      if (allocated(error)) return

      call err%clear()
      r = next_exponential_int(gen, EXPONENTIAL_MAX_MEAN, err)
      call check(error,.not. err%has_error(), "the largest accepted mean is accepted")
      if (allocated(error)) return
      call check(error, r >= 0_int64, "and does not overflow into a negative")
      if (allocated(error)) return

      r = next_exponential_int(gen, EXPONENTIAL_MAX_MEAN + 1_int64, err)
      call check(error, r == 0_int64, "one above the bound returns zero")
      if (allocated(error)) return
      call check(error, err%has_error(), "one above the bound sets an error")
   end subroutine test_exponential_edges

   subroutine test_exponential_shape(error)
      !! Mean and variance, and the memorylessness the tail treatment relies
      !! on. For an exponential the standard deviation equals the mean, so the
      !! sample mean has sigma = mean/sqrt(n).
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: i, above_one, above_two
      integer(int64) :: x
      real(dp) :: total, total_sq, mean, variance, sigma

      call gen%seed(SEED)
      total = 0.0_dp
      total_sq = 0.0_dp
      above_one = 0
      above_two = 0
      do i = 1, BULK
         x = next_exponential_int(gen, 1000_int64)
         total = total + real(x, dp)
         total_sq = total_sq + real(x, dp)**2
         if (x > 1000_int64) above_one = above_one + 1
         if (x > 2000_int64) above_two = above_two + 1
      end do

      mean = total/real(BULK, dp)
      variance = total_sq/real(BULK, dp) - mean**2
      sigma = 1000.0_dp/sqrt(real(BULK, dp))

      call check(error, abs(mean - 1000.0_dp) < 5.0_dp*sigma, &
                 "the sample mean is more than 5 sigma from the requested mean")
      if (allocated(error)) return
      ! variance of an exponential is mean**2; its estimator has a wider
      ! spread, so this is a loose band on purpose
      call check(error, abs(sqrt(variance) - 1000.0_dp) < 20.0_dp, &
                 "the standard deviation is not close to the mean")
      if (allocated(error)) return

      ! P(X > mean) = 1/e = 0.3679, P(X > 2 mean) = 1/e**2 = 0.1353
      call check(error, abs(real(above_one, dp)/real(BULK, dp) - 0.367879_dp) < 0.005_dp, &
                 "P(X > mean) is wrong")
      if (allocated(error)) return
      call check(error, abs(real(above_two, dp)/real(BULK, dp) - 0.135335_dp) < 0.005_dp, &
                 "P(X > 2 mean) is wrong, which is where a bad tail shows up")
   end subroutine test_exponential_shape

   subroutine test_exponential_small_mean(error)
      !! A mean of 2 is where truncating the fixed-point fraction instead of
      !! rounding it would show up as a 23% error.
      !!
      !! Pinned to the closed form rather than to 2.0. Discretising an
      !! exponential to integers leaves a residual bias that rounding does not
      !! remove: `E[round(m*X)] = exp(-1/(2m)) / (1 - exp(-1/m))`, which is
      !! 1.97932 at m = 2. The old band, +/-0.05 around 2.0, did catch
      !! truncation -- that gives 1.5415, well outside it -- but it pinned a
      !! value the algorithm cannot reach, and at 25 sigma wide it could not
      !! have caught a regression in the rounding term itself. This band is
      !! 5 sigma around what the algorithm is actually supposed to produce.
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int) :: i
      real(dp) :: total, mean

      call gen%seed(SEED)
      total = 0.0_dp
      do i = 1, BULK
         total = total + real(next_exponential_int(gen, 2_int64), dp)
      end do
      mean = total/real(BULK, dp)

      call check(error, abs(mean - EXPECTED_MEAN_AT_2) < 0.01_dp, &
                 "a small mean is off its closed-form expectation")
   end subroutine test_exponential_small_mean

   subroutine test_poisson_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      type(error_t) :: err
      integer(int32) :: c

      call gen%seed(SEED)

      c = next_poisson_int(gen, 0_default_int, err)
      call check(error, c == 0_int32, "a mean of zero counts zero")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), "a mean of zero is not an error")
      if (allocated(error)) return

      c = next_poisson_int(gen, -1_default_int, err)
      call check(error, c == 0_int32, "a negative mean returns zero")
      if (allocated(error)) return
      call check(error, err%has_error(), "a negative mean sets an error")
      if (allocated(error)) return

      call err%clear()
      c = next_poisson_int(gen, POISSON_MAX_MEAN_MILLI, err)
      call check(error,.not. err%has_error(), "the largest accepted mean is accepted")
      if (allocated(error)) return
      call check(error, c > 0_int32, "and counts something")
      if (allocated(error)) return

      c = next_poisson_int(gen, POISSON_MAX_MEAN_MILLI + 1_default_int, err)
      call check(error, c == 0_int32, "one above the bound returns zero")
      if (allocated(error)) return
      call check(error, err%has_error(), "one above the bound sets an error")
   end subroutine test_poisson_edges

   subroutine test_poisson_shape(error)
      !! For a Poisson the variance equals the mean, which is the property
      !! that separates it from any other count distribution with the same
      !! mean.
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int), parameter :: N = 200000_default_int
      integer(default_int) :: i
      integer(int32) :: c
      real(dp) :: total, total_sq, mean, variance, sigma

      call gen%seed(SEED)
      total = 0.0_dp
      total_sq = 0.0_dp
      do i = 1, N
         c = next_poisson_int(gen, 3500_default_int)
         total = total + real(c, dp)
         total_sq = total_sq + real(c, dp)**2
      end do

      mean = total/real(N, dp)
      variance = total_sq/real(N, dp) - mean**2
      sigma = sqrt(3.5_dp/real(N, dp))

      call check(error, abs(mean - 3.5_dp) < 5.0_dp*sigma, &
                 "the Poisson mean is more than 5 sigma out")
      if (allocated(error)) return
      call check(error, abs(variance - 3.5_dp) < 0.1_dp, &
                 "the variance does not match the mean, so this is not Poisson")
   end subroutine test_poisson_shape

   subroutine test_real_tier_shape(error)
      !! The real tier is not pinned -- it calls libm, and libm is not the
      !! same function on every compiler, which is the whole reason it lives
      !! in a separate module. Only its shape is checked.
      type(error_type), allocatable, intent(out) :: error
      type(splitmix64_t) :: gen
      integer(default_int), parameter :: N = 200000_default_int
      integer(default_int) :: i
      real(dp) :: x, total, total_sq, mean, variance

      call gen%seed(SEED)
      total = 0.0_dp
      total_sq = 0.0_dp
      do i = 1, N
         x = next_exponential_dp(gen, 4.0_dp)
         total = total + x
         total_sq = total_sq + x*x
         call check(error, x >= 0.0_dp, "an exponential deviate went negative")
         if (allocated(error)) return
      end do
      mean = total/real(N, dp)
      call check(error, abs(mean - 4.0_dp) < 5.0_dp*4.0_dp/sqrt(real(N, dp)), &
                 "the real exponential mean is more than 5 sigma out")
      if (allocated(error)) return

      call gen%seed(SEED)
      total = 0.0_dp
      total_sq = 0.0_dp
      do i = 1, N
         x = next_normal_dp(gen, 2.0_dp, 3.0_dp)
         total = total + x
         total_sq = total_sq + x*x
      end do
      mean = total/real(N, dp)
      variance = total_sq/real(N, dp) - mean**2

      call check(error, abs(mean - 2.0_dp) < 5.0_dp*3.0_dp/sqrt(real(N, dp)), &
                 "the normal mean is more than 5 sigma out")
      if (allocated(error)) return
      call check(error, abs(sqrt(variance) - 3.0_dp) < 0.05_dp, &
                 "the normal standard deviation is wrong")
   end subroutine test_real_tier_shape

end module test_pic_random_dist
