!! flop recorder
module pic_flop_recorder
   use pic_types
   implicit none

   type :: flop_recorder_type

      private
      integer(int64) :: flop_count = 0_int64

   contains

      procedure :: add => add_flops
      procedure :: get => get_flops
      procedure :: reset => reset_flop_counter

   end type flop_recorder_type

contains

   subroutine add_flops(self, flops)
      implicit none
      class(flop_recorder_type), intent(inout) :: self
      integer(int64), intent(in) :: flops
      self%flop_count = self%flop_count + flops

   end subroutine add_flops

   function get_flops(self) result(flops)
      implicit none
      class(flop_recorder_type), intent(in) :: self
      integer(int64) :: flops

      flops = self%flop_count

   end function get_flops

   subroutine reset_flop_counter(self)
      implicit none
      class(flop_recorder_type), intent(inout) :: self

      self%flop_count = 0_int64

   end subroutine reset_flop_counter

end module pic_flop_recorder
