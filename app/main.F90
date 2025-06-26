program main
   use jonquil, only: json_object, json_dump, json_error, json_array, &
                      set_value, add_array, json_serialize, json_value, json_loads, &
                      cast_to_object, get_value
   use pic_logger, only: global => global_logger, warning_level, verbose_level
   use pic_types
   use omp_lib
#ifdef USE_MPI
   use mpi_f08
#endif
   use pic_matrix_printer
   implicit none
   class(json_value), allocatable :: val
   type(json_object), pointer :: my_object
   type(json_array), pointer :: my_array
   type(json_error), allocatable :: error
   integer(default_int) :: ierr, rank, size, ival
   real(dp), dimension(:, :), allocatable :: A, B, C
   real(dp), dimension(:), allocatable :: C_flat
   integer(default_int) :: n, m, k, flat_size

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
      flat_size = m*n
      allocate (C_flat(flat_size))
      A = 1.0d0
      B = 1.0d0

      call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      C_flat = reshape(C, [flat_size])

      call print_matrix(C, "PLAIN")
      call json_loads(val, '{"a":1,"b":2}', error=error)
      if (allocated(error)) then
         print '(a)', error%message
         stop
      end if

      my_object => cast_to_object(val)
      if (associated(my_object)) then
         call get_value(my_object, "a", ival)
         print '(a,1x,i0)', "a is", ival

         call get_value(my_object, "b", ival)
         print '(a,1x,i0)', "b is", ival
      end if

      call add_array(my_object, "matrix", my_array)
      call set_value(my_array, C_flat)

      print *, json_serialize(my_object)

      call json_dump(my_object, "output.json", error=error)

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
