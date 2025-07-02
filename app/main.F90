program main
   !use jonquil, only: json_object, json_dump, json_error, json_array, &
   !                   set_value, add_array, json_serialize, json_value, json_loads, &
   !                   cast_to_object, get_value, add_object
   use pic_logger, only: global => global_logger, warning_level, verbose_level
   !use testdrive, only: run_testsuite
   use pic_types
   use pic_mpi
   use pic_matrix_printer
   use pic_timers
   implicit none
   type(pic_comm) :: comm
   !class(json_value), allocatable :: val
   !type(json_object), pointer :: my_object, new_object
   !type(json_array), pointer :: my_array, new_array
   !type(json_error), allocatable :: error
   integer(default_int) :: ierr, rank, size, ival
   real(dp), dimension(:, :), allocatable :: A, B, C
   real(dp), dimension(:), allocatable :: C_flat
   integer(default_int) :: n, m, k, flat_size

   call comm%init()

   if (comm%m_rank == 0) then
      print *, "This is the root process, rank: ", comm%m_rank

      ! block
      !   type(pic_timer) :: my_timer
      !   integer :: i
      !   integer, parameter :: n_loops = 200000000
      !   real(dp) :: s
      !   real(dp) :: elapsed_time
      !   s = 0.0_dp
      !   call my_timer%begin()
      !   do i = 1, n_loops
      !   s = s + sin(real(i, dp))
      !   end do
      !   call my_timer%finish()
      !   call my_timer%print_time()
      !   elapsed_time = my_timer%get_elapsed_time()
      !   print *, "My elpsed time is ", elapsed_time
      !   do i = 1, 5
      !     print *, s
      !   end do
      ! end block

      ! n = 4
      ! m = 4
      ! k = 4
      ! allocate (A(n, k), B(k, m), C(n, m))
      ! flat_size = m*n
      ! allocate (C_flat(flat_size))
      ! A = 1.0d0
      ! B = 1.0d0

      ! call dgemm('N', 'N', n, m, k, 1.0d0, A, n, B, k, 0.0d0, C, n)
      ! C_flat = reshape(C, [flat_size])

      ! call print_matrix(C, "PLAIN")

      ! call json_loads(val, '{"a":1,"b":2}', error=error)
      ! if (allocated(error)) then
      !    print '(a)', error%message
      !    stop
      ! end if

      ! my_object => cast_to_object(val)
      ! if (associated(my_object)) then
      !    call get_value(my_object, "a", ival)
      !    print '(a,1x,i0)', "a is", ival

      !    call get_value(my_object, "b", ival)
      !    print '(a,1x,i0)', "b is", ival
      ! end if

      ! block
      ! integer(default_int) :: i
      ! call add_array(my_object, "matrix", my_array)
      ! do i = 1, n
      ! !call add_object(my_array, new_object)
      ! call add_array(my_array, new_array)
      ! call set_value(new_array, C(i,:))
      ! !call add_array(my_object, to_string(i), my_array)
      ! !call set_value(my_array, C_flat)
      ! end do
      ! end block

      ! print *, json_serialize(my_object)

      ! call json_dump(my_object, "output.json", error=error)

      ! call global%configure(verbose_level)
      ! call global%configure_file_output('run.log', level=warning_level)

      ! call global%debug("This is a debug message")
      ! call global%verbose("This is a verbose message")
      ! call global%info("This is an info message")
      ! call global%performance("This is a performance message")
      ! call global%warning("This is a warning message")
      ! call global%error("This is an error message")

      ! call debug
      ! deallocate (A, B, C, C_flat)
      ! call global%close_log_file()
   else
      print *, "This is not the root process, rank: ", comm%m_rank
   end if
   call comm%finalize()
contains

   subroutine debug
      use pic_logger, only: global => global_logger
      implicit none

      call global%warning("Debugging the main program")
   end subroutine debug
end program main
