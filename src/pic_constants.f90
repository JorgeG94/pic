!! all things constants, physical, chemical, etc
module pic_constants
  !! this will be the physical constants module
   use pic_types, only: dp
   implicit none
   private
   real(dp), parameter, public :: gravity = 9.81_dp

end module pic_constants
