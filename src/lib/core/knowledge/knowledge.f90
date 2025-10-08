module pic_knowledge
   use pic_types, only: int32, dp
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge()
      character(len=:), allocatable :: knowledge(:)
      integer(int32) :: n, idx
      real(dp) :: r

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
