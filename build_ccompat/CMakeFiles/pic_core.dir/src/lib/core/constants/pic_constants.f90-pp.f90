# 1 "/home/runner/work/pic/pic/src/lib/core/constants/pic_constants.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/runner/work/pic/pic/build_ccompat//"
# 1 "/home/runner/work/pic/pic/src/lib/core/constants/pic_constants.f90"
! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! all things constants, physical, chemical, etc
module pic_constants
  !! this will be the physical constants module
   use pic_types, only: dp
   implicit none
   private
   real(dp), parameter, public :: gravity = 9.81_dp

end module pic_constants
