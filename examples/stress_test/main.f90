program main
   use pic_string_utils, only: to_string
   use pic_blas_interfaces, only: pic_gemm
   use pic_types, only: dp, int64, default_int
   use pic_timers, only: pic_timer
   use pic_flop_recorder, only: flop_recorder_type
   use pic_flop_rate, only: flop_rate_type
   use pic_mpi, only: pic_comm, MPI_SUM, MPI_MAX
   implicit none
   type(pic_comm) :: comm
   type(flop_rate_type) :: pic_flops
   integer(default_int) :: ierr, rank, size
   integer(int64) :: flops, total_flops
   real(dp) :: max_time
   real(dp) :: elapsed_time
   real(dp) :: flop_rate

   ! this thing has the comm world
   call comm%init(from_world=.true.)

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
      print *, "TIME is "//to_string(elapsed_time)//" seconds in rank "//to_string(comm%m_rank)

   end block

   ! Global reduction
   call comm%reduce(flops, total_flops, 1, MPI_SUM, 0)
   call comm%reduce(elapsed_time, max_time, 1, MPI_MAX, 0)

   if (comm%m_rank == 0) then
      flop_rate = real(total_flops, dp)/max_time/1.0e9_dp
      print *, "Global time: ", max_time, " seconds"
      print *, "Total GFLOPs: ", total_flops/1.0e9_dp
      print *, "Global FLOP rate: ", flop_rate, " GFLOP/s"
   end if

   call comm%finalize()

end program main
