program main
   use pic_logger, only: global => global_logger, warning_level, verbose_level
   use pic_types
   use omp_lib
#ifdef USE_MPI
   use mpi_f08
#endif
   use pic_matrix_printer
   implicit none
   integer(default_int) :: ierr, rank, size
   real(dp), dimension(:, :), allocatable :: A, B, C
   integer(default_int) :: n, m, k

#ifdef USE_MPI
   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
#else
   rank = 0
#endif
   if (rank == 0) then
      print *, "This is the root process, rank: ", rank, " with ", omp_get_max_threads(), " threads"
      n = 4
      m = 4
      k = 4
      allocate (A(n, k), B(k, m), C(n, m))
      A = 1.0d0
      B = 1.0d0

      call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)

      call print_matrix(C, "PLAIN")

      call global%configure(verbose_level)
      call global%configure_file_output('run.log', level=warning_level)

      call global%debug("This is a debug message")
      call global%verbose("This is a verbose message")
      call global%info("This is an info message")
      call global%performance("This is a performance message")
      call global%warning("This is a warning message")
      call global%error("This is an error message")

      call debug
      deallocate (A, B, C)
      call global%close_log_file()
   else
      print *, "This is not the root process, rank: ", rank
   end if
#ifdef USE_MPI
   call mpi_finalize(ierr)
#endif
contains

   subroutine debug
      use pic_logger, only: global => global_logger
      implicit none

      call global%warning("Debugging the main program")
   end subroutine debug
end program main
