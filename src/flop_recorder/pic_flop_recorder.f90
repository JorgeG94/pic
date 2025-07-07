!! flop recorder
module pic_flop_recorder
  !! general flop recorder module
   use pic_types
   implicit none

   type :: flop_recorder_type
    !! the flop recorder type simply contains a flop count
    !! this should be the largest possible integer in the planet
    !! currently this will overflow for zetta flops

      private
      integer(int64) :: flop_count = 0_int64

   contains

      procedure :: add => add_flops
      procedure :: get => get_flops
      procedure :: reset => reset_flop_counter

   end type flop_recorder_type

contains

   subroutine add_flops(self, flops)
    !! add the FLOPs!
      class(flop_recorder_type), intent(inout) :: self
      integer(int64), intent(in) :: flops
      self%flop_count = self%flop_count + flops

   end subroutine add_flops

   function get_flops(self) result(flops)
    !! return the FLOPs at a given point in time
      class(flop_recorder_type), intent(in) :: self
      integer(int64) :: flops

      flops = self%flop_count

   end function get_flops

   subroutine reset_flop_counter(self)
    !! reset the FLOP the counter
      class(flop_recorder_type), intent(inout) :: self

      self%flop_count = 0_int64

   end subroutine reset_flop_counter

end module pic_flop_recorder
