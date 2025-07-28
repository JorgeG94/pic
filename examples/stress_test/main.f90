program main
   use pic_string_utils, only: to_string
   use pic_blas_interfaces, only: pic_gemm
   use pic_types, only: dp, int64, default_int
   use pic_timer, only: pic_timer_type
   use pic_flop_recorder, only: flop_recorder_type
   use pic_flop_rate, only: flop_rate_type
   use mpi_f08, only: MPI_COMM_WORLD, MPI_Init, MPI_Finalize, &
                      MPI_Comm_rank, MPI_Comm_size, MPI_Reduce, MPI_INTEGER8, &
                      MPI_DOUBLE_PRECISION, MPI_MAX, MPI_SUM
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
      call pic_gemm(A, B, C)
      call pic_flops%start_time()
      do i = 1, n_loops
         curr_flops = 2*m*n*k
         call pic_flops%add_flops(curr_flops)
         call pic_gemm(A, B, C)
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
      flop_rate = real(total_flops, dp)/max_time/1.0e9_dp
      ! you can also get the FLOP rate like this and use it for something
      print *, "Global time: ", max_time, " seconds"
      print *, "Total GFLOPs: ", total_flops/1.0e9_dp
      print *, "Global FLOP rate: ", flop_rate, " GFLOP/s"
      !call pic_flops%report()
   end if

   call MPI_Finalize(ierr)

end program main
