! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! flop recorder
module pic_flop_recorder
  !! general flop recorder module
   use pic_types, only: int64
   implicit none
   private
   public :: flop_recorder_type

   type :: flop_recorder_type
    !! the flop recorder type simply contains a flop count
    !! this should be the largest possible integer in the planet
    !! currently this will overflow for zetta flops

      private
      integer(int64) :: flop_count = 0_int64

   contains

      procedure, non_overridable :: add => add_flops
      procedure, non_overridable :: get => get_flops
      procedure, non_overridable :: reset => reset_flop_counter

   end type flop_recorder_type

contains

   subroutine add_flops(self, flops)
   !! Add an int64 variable which contains the FLOPs
   !! we use int64 since we might reach very large FLOP counts
   !!
   !! Usage: call my_flop_recorder%add(flops)
   !!
      class(flop_recorder_type), intent(inout) :: self
      integer(int64), intent(in) :: flops
      self%flop_count = self%flop_count + flops

   end subroutine add_flops

   function get_flops(self) result(flops)
    !! Get the int64 number of FLOPs we currently have in the counter
    !!
    !! Usage: flops = my_flop_recorder%get()
    !!
      class(flop_recorder_type), intent(in) :: self
      integer(int64) :: flops

      flops = self%flop_count

   end function get_flops

   subroutine reset_flop_counter(self)
    !! Resets the flop counter to 0_int64
    !!
    !! Usage: call my_flop_recorder%reset()
    !!
      class(flop_recorder_type), intent(inout) :: self

      self%flop_count = 0_int64

   end subroutine reset_flop_counter

end module pic_flop_recorder
