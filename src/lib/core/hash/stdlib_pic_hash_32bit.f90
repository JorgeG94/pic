
module stdlib_pic_hash_32bit

   use stdlib_pic_hash_32bit_fnv, only: fnv_1_hash, fnv_1a_hash
   use pic_types, only: &
      dp, &
      int32, &
      int64

   implicit none

   private

! pow32_over_phi is the odd integer that most closely approximates 2**32/phi,
! where phi is the golden ratio 1.618...
   integer(int32), parameter :: &
      pow32_over_phi = int(z'9E3779B9', int32)

   public :: &
      fibonacci_hash, &
      fnv_1_hash, &
      fnv_1a_hash, &
      odd_random_integer, &
      universal_mult_hash

contains

   elemental function fibonacci_hash(key, nbits) result(sample)
!! Version: experimental
!!
!! Maps the 32 bit integer `key` to an unsigned integer value with only `nbits`
!! bits where `nbits` is less than 32
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fibonacci_hash-maps-an-integer-to-a-smaller-number-of-bits))

      integer(int32), intent(in) :: key
      integer, intent(in)        :: nbits
      integer(int32)             :: sample

      sample = ishft(key*pow32_over_phi, -32 + nbits)

   end function fibonacci_hash

   elemental function universal_mult_hash(key, seed, nbits) result(sample)
!! Version: experimental
!!
!! Uses the "random" odd 32 bit integer `seed` to map the 32 bit integer `key` to
!! an unsigned integer value with only `nbits` bits where `nbits` is less than 32
!! ([Specification](../page/specs/stdlib_hash_procedures.html#universal_mult_hash-maps-an-integer-to-a-smaller-number-of-bits))
      integer(int32), intent(in) :: key
      integer(int32), intent(in) :: seed
      integer, intent(in)        :: nbits
      integer(int32)             :: sample

      sample = ishft(key*seed, -32 + nbits)

   end function universal_mult_hash

   subroutine odd_random_integer(harvest)
!! Version: experimental
!!
!! Returns a 32 bit pseudo random integer, `harvest`, distributed uniformly over
!! the odd integers of the `int32` kind.
!! ([Specification](../page/specs/stdlib_hash_procedures.html#odd_random_integer-returns-an-odd-integer))
      integer(int32), intent(out) :: harvest
      real(dp) :: sample

      call random_number(sample)
      harvest = int(floor(sample*2_int64**32, int64) - 2_int64**31, &
                    int32)
      harvest = ishft(harvest, 1) + 1_int32

   end subroutine odd_random_integer

end module stdlib_pic_hash_32bit
