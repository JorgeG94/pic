program main
   use pic_string_utils
   use pic_blas_interfaces
   use pic_types
   use pic_timers
   use pic_flop_recorder
   use pic_flop_rate
   use mpi_f08
   implicit none
   integer(default_int) :: ierr, rank, size
   !integer(default_int) :: n, m, k, flat_size
   integer(int64) :: flops, total_flops
   !type(flop_recorder_type) :: pic_flops
   type(flop_rate_type) :: pic_flops
   real(dp) :: max_time
   real(dp) :: elapsed_time
   real(dp) :: flop_rate

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

   flops = 0_int64
   total_flops = 0_int64

   block
      real(dp), dimension(:, :), allocatable :: A, B, C
      integer, parameter :: n_loops = 10
      integer, parameter :: m_size = 1024
      integer, parameter :: m = m_size
      integer, parameter :: n = m_size
      integer, parameter :: k = m_size
      integer(int64) :: curr_flops
      integer :: i
      allocate (A(n, k), B(k, m), C(n, m))
      A = 1.0_dp
      B = 1.0_dp
      C = 0.0_dp
      !call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      call pic_gemm(A, B, C)
      !call my_timer%start()
      call pic_flops%start_time()
      do i = 1, n_loops
         curr_flops = 2*m*n*k
         call pic_flops%add_flops(curr_flops)
         !   call pic_gemm(A,B,C)
         call pic_gemm(A, B, C)
         !call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      end do
      call pic_flops%stop_time()
      elapsed_time = pic_flops%get_time()
      flops = pic_flops%get_flops()
      print *, "TIME is "//to_string(elapsed_time)//" seconds in rank "//to_string(rank)

   end block

   ! Global reduction
   call MPI_Reduce(flops, total_flops, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
   call MPI_Reduce(elapsed_time, max_time, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

   if (rank == 0) then
      !flop_rate = pic_flops%get_flop_rate()
      flop_rate = real(total_flops,dp) / max_time / 1.0d9
      ! you can also get the FLOP rate like this and use it for something
      print *, "Global time: ", max_time, " seconds"
      print *, "Total GFLOPs: ", total_flops/1d9
      print *, "Global FLOP rate: ", flop_rate, " GFLOP/s"
      !call pic_flops%report()
   end if

   call MPI_Finalize(ierr)

end program main
