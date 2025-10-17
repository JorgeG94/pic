!! the gathered knowledge of the species
module pic_knowledge
  !! a simple module that collects phrases and prints them out randomly, like fortune
   use pic_types, only: int32, dp
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge()
    !! I print random knowledge
      character(len=64), allocatable :: knowledge(:)
      integer(int32) :: n, idx
      real(dp) :: r
      allocate (knowledge(3))

      knowledge = [ &
                  "The long line! (CS,2023)                                        ", &
                  "Maybe I have the Fortran brain-rot in Dijkstra's words (IP,2025)", &
                  "Mojo, yes, do I approve it, no. (IP, 2025)                      " &
                  ]

      n = size(knowledge)

      call random_number(r)
      idx = int(r*n) + 1
      if (idx > n) idx = n

      print *, 'Random knowledge: ', trim(knowledge(idx))

   end subroutine get_knowledge

end module pic_knowledge
