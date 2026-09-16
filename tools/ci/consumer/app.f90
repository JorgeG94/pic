! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
program consumer_app
   !! Uses one name from each layer a consumer is likely to reach for,
   !! including the terminal layer, so the check fails if any of them cannot
   !! be resolved from a nested build rather than only if configure does.
   use pic_types, only: default_int
   use pic_term, only: term_is_tty, TERM_STDIN
   use pic_vector, only: vector_int32_t
   implicit none

   type(vector_int32_t) :: v
   integer(default_int) :: n

   call v%push_back(42)
   n = v%size()
   write (*, "(a,i0)") "vector size: ", n
   write (*, "(a,l1)") "stdin is a tty: ", term_is_tty(TERM_STDIN)
end program consumer_app
