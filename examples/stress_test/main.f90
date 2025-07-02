program main
   use pic_string_utils
   use pic_types
   use pic_timers
   use mpi_f08
   implicit none
   integer(default_int) :: ierr, rank, size, ival
   real(dp), dimension(:), allocatable :: C_flat
   integer(default_int) :: n, m, k, flat_size
   integer(int64) :: flops, total_flops
   real(dp) :: max_time
   real(dp) :: elapsed_time
   real(dp) :: flop_rate

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

   flops = 0_int64
   total_flops = 0_int64

   block
      type(pic_timer) :: my_timer
      real(dp), dimension(:, :), allocatable :: A, B, C
      integer, parameter :: n_loops = 10
      integer, parameter :: m_size = 1024
      integer, parameter :: m = m_size
      integer, parameter :: n = m_size
      integer, parameter :: k = m_size
      integer :: i
      allocate (A(n, k), B(k, m), C(n, m))
      A = 1.0_dp
      B = 1.0_dp
      C = 0.0_dp
      call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      call my_timer%start()
      do i = 1, n_loops
         flops = flops + 2*m*n*k
         call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      end do
      call my_timer%end()
      elapsed_time = my_timer%get_elapsed_time()
      !print *, "TIME is " // to_string(elapsed_time) // " seconds in rank "//to_string(rank)

   end block

   ! Global reduction
   call MPI_Reduce(flops, total_flops, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
   call MPI_Reduce(elapsed_time, max_time, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

   if (rank == 0) then
      flop_rate = real(total_flops, dp)/max_time/1.0d9
      print *, "Global time: ", max_time, " seconds"
      print *, "Total GFLOPs: ", total_flops/1d9
      print *, "Global FLOP rate: ", flop_rate, " GFLOP/s"
   end if

   call MPI_Finalize(ierr)

end program main
