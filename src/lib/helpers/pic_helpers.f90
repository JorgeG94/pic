!! General helpers
module pic_test_helpers
  !! simple reusable helpers for random things
   use pic_types, only: int64, dp, default_int
   implicit none

   private
   public :: dummy_work

contains
   subroutine dummy_work()
    !! this subroutine runs a random dgemm to create work so that timers and other testing utils work nicely
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
