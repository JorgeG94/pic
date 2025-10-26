!! the gathered knowledge of the species
module pic_knowledge
  !! a simple module that collects phrases and prints them out randomly, like fortune
   use pic_types, only: int32, dp
   use pic_logger, only: logger => global_logger
   use pic_stdlib_string_type, only: string_type, assignment(=), char
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge()
    !! I print random knowledge
      type(string_type), allocatable :: knowledge(:)
      type(string_type) :: tmp
      integer(int32) :: n, idx
      real(dp) :: r
      allocate (knowledge(8))

      knowledge(1) = "The long line! (CS,2023)"
      knowledge(2) = "Maybe I have the Fortran brain-rot in Dijkstra's words (IP,2025)"
      knowledge(3) = "Mojo, yes, do I approve it, no. (IP, 2025)"
      knowledge(4) = "No, rice does not contain gluten (EG, dawn of time)"
      knowledge(5) = "Yes, potatoes are gluten free (EG, dawn of time)"
      knowledge(6) = "Stonks (MS, 2019)"
      knowledge(7) = "Praise the machine god"
      knowledge(8) = "Maybe I WILL use a more efficient language: Fortran (Jorge, to CS)"

      n = size(knowledge)

      call random_number(r)
      idx = int(r*n) + 1
      if (idx > n) idx = n

      call logger%knowledge(trim(char(knowledge(idx))))

   end subroutine get_knowledge

end module pic_knowledge
