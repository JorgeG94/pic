program main
   use pic_logger, only: global => global_logger, warning_level
   use pic_types
   use omp_lib
   use mpi_f08
   use pic_matrix_printer
   implicit none

   real(dp), dimension(:, :), allocatable :: A, B, C
   integer(default_int) :: n, m, k

   n = 4
   m = 4
   k = 4
   allocate (A(n, k), B(k, m), C(n, m))
   A = 1.0d0
   B = 1.0d0

   call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)

   call print_matrix(C, "PLAIN")

   call global%configure(warning_level)

   call global%debug("This is a debug message")
   call global%verbose("This is a verbose message")
   call global%info("This is an info message")
   call global%performance("This is a performance message")
   call global%warning("This is a warning message")
   call global%error("This is an error message")

   call debug
contains

   subroutine debug
      use pic_logger, only: global => global_logger
      implicit none

      call global%warning("Debugging the main program")
   end subroutine debug
end program main
