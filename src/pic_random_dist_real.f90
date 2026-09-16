! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Real-valued random distributions, for tooling and statistics.
module pic_random_dist_real
   !! Distributions that return `real(dp)`.
   !!
   !! ### Read this before using these in simulation state
   !!
   !! These call `log`, `sqrt` and `cos`. Those are libm functions, and libm
   !! is not one function: GNU, Intel, NVIDIA, LLVM and LFortran do not agree
   !! to the last bit on any of them, and nothing obliges them to. Two runs of
   !! the same code with the same seed on two compilers will therefore produce
   !! different values here.
   !!
   !! That is fine for a histogram, a plot, a convergence study or a
   !! back-of-the-envelope estimate. It is not fine for anything that has to
   !! be compared across runs or across machines -- a checkpoint digest, a
   !! determinism log, a bisected divergence. For those, use
   !! `pic_random_dist`, whose every routine is integer-only and pinned by
   !! tests.
   !!
   !! The two tiers are separate modules precisely so that this is a decision
   !! rather than an accident: code that never names this module cannot reach
   !! a non-reproducible deviate.
   !!
   !! ### Quick start
   !!
   !! ```fortran
   !! use pic_rng, only: splitmix64_t
   !! use pic_random_dist_real, only: next_exponential_dp, next_normal_dp
   !!
   !! type(splitmix64_t) :: gen
   !! call gen%seed(20250914_int64)
   !!
   !! wait = next_exponential_dp(gen, 4.0_dp)          ! mean 4
   !! noise = next_normal_dp(gen, 0.0_dp, 1.0_dp)      ! standard normal
   !! ```
   use pic_types, only: dp
   use pic_rng, only: splitmix64_t, pcg32_t, next_real_dp
   implicit none
   private

   public :: next_exponential_dp
   public :: next_normal_dp

   real(dp), parameter :: TWO_PI = 6.283185307179586476925286766559_dp
      !! 2*pi to more digits than `real(dp)` can hold, so the nearest
      !! representable value is selected rather than a rounded decimal.

   interface next_exponential_dp
      !! Draw an exponential deviate with the given mean.
      !!
      !! Inverse transform: `-mean * log(u)` with `u` uniform in (0, 1]. The
      !! uniform is taken as `1 - next_real_dp(gen)` rather than
      !! `next_real_dp(gen)` because the latter can return exactly zero, and
      !! `log(0)` is minus infinity. Subtracting from one moves the excluded
      !! endpoint to the other side, where it is harmless.
      !!
      !! A negative `mean` is not rejected; it simply mirrors the distribution
      !! about zero, which is occasionally what a caller wants.
      module procedure splitmix64_next_exponential_dp
      module procedure pcg32_next_exponential_dp
   end interface next_exponential_dp

   interface next_normal_dp
      !! Draw a normal deviate with mean `mu` and standard deviation `sigma`.
      !!
      !! Box-Muller. The transform naturally yields two independent deviates
      !! per pair of uniforms, and the usual implementation caches the second
      !! for the next call. This one discards it.
      !!
      !! That halves the throughput and is deliberate. A cache makes the
      !! generator state depend on whether the call count is odd or even, so
      !! two runs that draw the same number of normals but interleave them
      !! differently with other draws diverge. Without it, the generator's
      !! position is a plain function of how many times it has been asked for
      !! anything, which is far easier to reason about -- and it keeps this
      !! function `pure` in spirit: no hidden state between calls.
      module procedure splitmix64_next_normal_dp
      module procedure pcg32_next_normal_dp
   end interface next_normal_dp

contains

   function splitmix64_next_exponential_dp(gen, mean) result(r)
      !! Exponential deviate from a SplitMix64 generator.
      type(splitmix64_t), intent(inout) :: gen
         !! Generator, advanced by one draw.
      real(dp), intent(in) :: mean
         !! Mean of the distribution.
      real(dp) :: r

      r = -mean*log(1.0_dp - next_real_dp(gen))
   end function splitmix64_next_exponential_dp

   function pcg32_next_exponential_dp(gen, mean) result(r)
      !! Exponential deviate from a PCG32 generator.
      type(pcg32_t), intent(inout) :: gen
         !! Generator, advanced by one draw.
      real(dp), intent(in) :: mean
         !! Mean of the distribution.
      real(dp) :: r

      r = -mean*log(1.0_dp - next_real_dp(gen))
   end function pcg32_next_exponential_dp

   function splitmix64_next_normal_dp(gen, mu, sigma) result(r)
      !! Normal deviate from a SplitMix64 generator.
      type(splitmix64_t), intent(inout) :: gen
         !! Generator, advanced by exactly two draws.
      real(dp), intent(in) :: mu
         !! Mean.
      real(dp), intent(in) :: sigma
         !! Standard deviation.
      real(dp) :: r

      real(dp) :: u1, u2

      u1 = 1.0_dp - next_real_dp(gen)
      u2 = next_real_dp(gen)
      r = mu + sigma*sqrt(-2.0_dp*log(u1))*cos(TWO_PI*u2)
   end function splitmix64_next_normal_dp

   function pcg32_next_normal_dp(gen, mu, sigma) result(r)
      !! Normal deviate from a PCG32 generator.
      type(pcg32_t), intent(inout) :: gen
         !! Generator, advanced by exactly two draws.
      real(dp), intent(in) :: mu
         !! Mean.
      real(dp), intent(in) :: sigma
         !! Standard deviation.
      real(dp) :: r

      real(dp) :: u1, u2

      u1 = 1.0_dp - next_real_dp(gen)
      u2 = next_real_dp(gen)
      r = mu + sigma*sqrt(-2.0_dp*log(u1))*cos(TWO_PI*u2)
   end function pcg32_next_normal_dp

end module pic_random_dist_real
