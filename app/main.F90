program main
   use pic, only: pic_print_banner
   use pic_knowledge, only: get_knowledge 
   implicit none

   call pic_print_banner()

   call get_knowledge()

end program main
