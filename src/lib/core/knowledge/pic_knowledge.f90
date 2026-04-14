! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! the gathered knowledge of the species
module pic_knowledge
  !! a simple module that collects phrases and prints them out randomly, like fortune
   use pic_types, only: int32, dp
   use pic_logger, only: logger => global_logger
   use pic_string_type, only: string_type, assignment(=), char
   implicit none

   private

   public :: get_knowledge

contains

   subroutine get_knowledge(print_all)
    !! I print random knowledge
      logical, optional, intent(in) :: print_all
      type(string_type), allocatable :: knowledge(:)
      integer(int32) :: n, idx
      real(dp) :: r
      allocate (knowledge(44))

      knowledge(1) = "The long line! (CS,2023)"
      knowledge(2) = "Maybe I have the Fortran brain-rot in Dijkstra's words (IP,2025)"
      knowledge(3) = "Mojo, yes, do I approve it, no. (IP, 2025)"
      knowledge(4) = "No, rice does not contain gluten (EG, dawn of time)"
      knowledge(5) = "Yes, potatoes are gluten free (EG, dawn of time)"
      knowledge(6) = "Stonks (MS, 2019)"
      knowledge(7) = "Praise the machine god"
      knowledge(8) = "Maybe I WILL use a more efficient language: Fortran (Jorge, to CS)"
      knowledge(9) = "No one owns Makefiles!"
      knowledge(10) = "Why did you choose CMake over any other build system? Jorge: Stockholm Syndrome?"
      knowledge(11) = "That is exactly what Bjarne Sostrup intended when he created C++ - in response to RS doing an abomination in C++, by CS"
      knowledge(12) = "lmao yes vibe climate coding, (MW, private communication 2026)"
      knowledge(13) = "so a lot of it is very informed physical vibes and we can verify the vibes vibe, (Jorge, 2026)"
      knowledge(14) = "I now understand why my understanding was not right (MW,2025)"
      knowledge(15) = "I imagine the results are usefully unphysical (MW, 2025)"
      knowledge(16) = "Hmm I just got notified that the do not merge branch, got merged"
      knowledge(17) = "We have no ETA. We wait. (Pawsey, 2024)"
      knowledge(18) = "Dale dug a hole. Tell 'em Dale!"
      knowledge(19) = "Tell him he's dreamin!"
      knowledge(20) = "On what constitutional basis do you base your arguments? The vibes, your honour"
      knowledge(21) = "Unfortunately life isn't quadratic - Schlegel"
      knowledge(22) = "In stressful situations, please resort to eating chocolate. (Fazeleh's life mantra)"
      knowledge(23) = "There are three constants in life. Death, Taxes, and Vim."
      knowledge(24) = "A tri-triathlon, you do a triathlon...three times!"
      knowledge(25) = "I've been caught like the tiger of Santa Julia (Mexican saying)"
      knowledge(26) = "Shrimp that falls asleep is carried away by the current (Mexican saying)"
      knowledge(27) = "So you can see on which side the iguana bites (Mexican saying)"
      knowledge(28) = "*mexican whistle of approval* (Mexican kinda saying)"
      knowledge(29) = "Let's dig in that is mole de olla (Mexican saying)"
      knowledge(30) = "Para todo mal, mezcal; y para todo bien, también"
      knowledge(31) = "The broth ended up more expensive than the meatballs (mexican saying)"
      knowledge(32) = "When you're just going to the butcher, I'm already frying the milanesas (mexican saying)"
      knowledge(33) = "You've added too much cream to your tacos (Mexican saying)"
      knowledge(34) = "The chahuitsle already fell on us (Mexican saying)"
      knowledge(35) = "The clown has carried us (Mexican saying)"
      knowledge(36) = "The devil knows more because he's old, not because he's the devil (Mexican saying)"
      knowledge(37) = "Oh no, they've flipped the tortilla on me (Mexican saying)"
      knowledge(38) = "Gotta measure the water of the sweet potatoes (Mexican saying)"
      knowledge(39) = "It has smoked a lighthouse (Mexican saying)"
      knowledge(40) = "The sun burns very much (Mexican saying)"
      knowledge(41) = "Australia went to war against the Emus in 1932, this event is called the Great Emu War. Australia lost. "
      knowledge(42) = "Ah you got a shower. (Edgar's dad, the ancient Mexican scriptures)"
      knowledge(43) = "4 tomatillos, 2 serranos, salt, pepper, 5 cloves of garlic, half an onion and a pinch of cilantro."
      knowledge(44) = "Life is short, make it shorter. (Jorge, talking about butter pumps at American cinemas)"

      n = size(knowledge)

      call random_number(r)
      idx = int(r*n) + 1
      if (idx > n) idx = n

      call logger%knowledge(trim(char(knowledge(idx))))

      if (present(print_all)) then
         if (print_all) then
            block
               integer(int32) :: i
               do i = 1, size(knowledge)
                  call logger%knowledge(trim(char(knowledge(i))))
               end do
            end block
         end if
      end if

   end subroutine get_knowledge

end module pic_knowledge
