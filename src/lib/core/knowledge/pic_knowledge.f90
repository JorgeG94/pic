!! the gathered knowledge of the species
module pic_knowledge
  !! a simple module that collects phrases and prints them out randomly, like fortune
   use pic_types, only: int32, dp
   use pic_logger, only: logger => global_logger
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge()
    !! I print random knowledge
      character(len=66), allocatable :: knowledge(:)
      integer(int32) :: n, idx
      real(dp) :: r
      allocate (knowledge(7))

      knowledge = [ &
                  "The long line! (CS,2023)                                          ", &
                  "Maybe I have the Fortran brain-rot in Dijkstra's words (IP,2025)  ", &
                  "Mojo, yes, do I approve it, no. (IP, 2025)                        ", &
                  "No, rice does not contain gluten (EG, dawn of time)               ", &
                  "Yes, potatoes are gluten free (EG, dawn of time)                  ", &
                  "Stonks (MS, 2019)                                                 ", &
                  "Praise the machine god                                            ", &
                  "Maybe I WILL use a more efficient language: Fortran (Jorge, to CS)" &
                  ]

      n = size(knowledge)

      call random_number(r)
      idx = int(r*n) + 1
      if (idx > n) idx = n

      call logger%knowledge(trim(knowledge(idx)))

   end subroutine get_knowledge

end module pic_knowledge
