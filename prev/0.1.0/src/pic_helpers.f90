! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! General helpers
module pic_test_helpers
  !! simple reusable helpers for random things
   use pic_types, only: int64, dp, default_int, sp
   use pic_global_definitions, only: tol_sp, tol_dp
   implicit none

   private
   public :: dummy_work, is_equal

   interface is_equal
      !! The is equal interface is used to circumvent the fact that Fortran, rightfully, complains about
      !! comparing two reals without involving a tolerance, i.e. if(a == b) since there is no guarantee that
      !! the two reals are exactly equal due to floating point precision issues. The functions are defined as elemental
      !!
      !! Usage: if (is_equal(a, b)) then
      !! where a and b are real numbers, and the function will return true if they are
      !! equal within the tolerance defined in pic_global_definitions.
      module procedure is_equal_sp
      module procedure is_equal_dp
   end interface is_equal

contains

   elemental function is_equal_sp(a, b) result(res)
      real(sp), intent(in) :: a, b
      logical :: res

      res = abs(a - b) < tol_sp
   end function is_equal_sp

   elemental function is_equal_dp(a, b) result(res)
      real(dp), intent(in) :: a, b
      logical :: res

      res = abs(a - b) < tol_dp
   end function is_equal_dp

   subroutine dummy_work()
    !! this subroutine runs a random dgemm to create work so that timers and other testing utils work nicely
    !!
    !! Usage: call dummy_work()
    !!
    !! it will simply do a 256 by 256 dgemm, woo
      integer(int64) ::  i, j, k
      integer(default_int) :: m
      real(dp), allocatable :: A(:, :), B(:, :), C(:, :)

      m = 256_default_int
      allocate (A(m, m), B(m, m), C(m, m))
      A = 1.0_dp
      B = 1.0_dp
      C = 0.0_dp
      do i = 1, m
         do j = 1, m
            do k = 1, m

               C(i, j) = C(i, j) + A(i, k)*B(k, j)
            end do
         end do
      end do
      print *, C(1, 1)
      deallocate (A, B, C)
   end subroutine dummy_work
end module pic_test_helpers
