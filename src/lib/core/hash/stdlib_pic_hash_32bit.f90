
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
      fnv_1_hash, &
      fnv_1a_hash

end module stdlib_pic_hash_32bit
