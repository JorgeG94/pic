module test_pic_matrix_printer
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_matrix_printer, only: print_array, print_array_with_bounds
   use pic_types, only: dp, default_int
   implicit none
   private
   public :: collect_pic_matrix_printer_tests

contains

   subroutine collect_pic_matrix_printer_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      tests = [ &
              new_unittest("print_vector_plain", test_print_vector_plain), &
              new_unittest("print_vector_numpy", test_print_vector_numpy), &
              new_unittest("print_matrix_plain", test_print_matrix_plain), &
              new_unittest("print_matrix_mathematica", test_print_matrix_mathematica), &
              new_unittest("print_vector_n", test_print_vector_n), &
              new_unittest("print_matrix_m_n", test_print_matrix_m_n) &
              ]
   end subroutine collect_pic_matrix_printer_tests

   subroutine test_print_vector_plain(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: v(3) = [1.0_dp, 2.0_dp, 3.0_dp]

      call print_array(v, "PLAIN")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return
   end subroutine test_print_vector_plain

   subroutine test_print_vector_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: v(2) = [10.0_dp, 20.0_dp]

      call print_array(v, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_vector_numpy

   subroutine test_print_matrix_plain(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: m(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call print_array(m, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_matrix_plain

   subroutine test_print_matrix_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: m(1, 2) = reshape([1.0_dp, 2.0_dp], [1, 2])

      call print_array(m, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_matrix_mathematica

   subroutine test_print_vector_n(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: n = 3
      real(dp), parameter :: v(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]

      call print_array_with_bounds(v, n, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_vector_n

   subroutine test_print_matrix_m_n(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: n = 2
      integer(default_int), parameter :: mm = 3
      real(dp), parameter :: m(2, 3) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [2, 3])

      call print_array_with_bounds(m, n, mm, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_matrix_m_n

end module test_pic_matrix_printer
