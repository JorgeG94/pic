!! the gathered knowledge of the species
module pic_knowledge
  !! a simple module that collects phrases and prints them out randomly, like fortune
   use pic_types, only: int32, dp
   use pic_logger, only: logger => global_logger
   use pic_string_mod, only: pic_string_type
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge()
    !! I print random knowledge
      type(pic_string_type), allocatable :: knowledge(:)
      integer(int32) :: n, idx
      real(dp) :: r
      allocate (knowledge(8))

      call knowledge(1)%assign("The long line! (CS,2023)")
      call knowledge(2)%assign("Maybe I have the Fortran brain-rot in Dijkstra's words (IP,2025)")
      call knowledge(3)%assign("Mojo, yes, do I approve it, no. (IP, 2025)")
      call knowledge(4)%assign("No, rice does not contain gluten (EG, dawn of time)")
      call knowledge(5)%assign("Yes, potatoes are gluten free (EG, dawn of time)")
      call knowledge(6)%assign("Stonks (MS, 2019)")
      call knowledge(7)%assign("Praise the machine god")
      call knowledge(8)%assign("Maybe I WILL use a more efficient language: Fortran (Jorge, to CS)")

      n = size(knowledge)

      call random_number(r)
      idx = int(r*n) + 1
      if (idx > n) idx = n

      call logger%knowledge(trim(knowledge(idx)%to_char()))

   end subroutine get_knowledge

end module pic_knowledge
