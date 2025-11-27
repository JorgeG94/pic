
module test_pic_matrix_printer_v2
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_array, only: pic_print_array
   use pic_types, only: sp, dp, int32, int64, default_int
   implicit none
   private
   public :: collect_pic_matrix_printer_v2_tests

contains

   subroutine collect_pic_matrix_printer_v2_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)

      tests = [ &
              new_unittest("print_vector_plain", test_print_vector_plain), &
              new_unittest("print_vector_numpy", test_print_vector_numpy), &
              new_unittest("print_vector_mathematica", test_print_vector_mathematica), &
              new_unittest("print_packed_matrix_plain", test_print_packed_matrix_plain), &
              new_unittest("print_packed_matrix_numpy", test_print_packed_matrix_numpy), &
              new_unittest("print_packed_matrix_mathematica", test_print_packed_matrix_mathematica), &
              new_unittest("print_matrix_plain", test_print_matrix_plain), &
              new_unittest("print_matrix_numpy", test_print_matrix_numpy), &
              new_unittest("print_matrix_mathematica", test_print_matrix_mathematica), &
              new_unittest("print_3d_tensor_plain", test_print_3d_tensor_plain), &
              new_unittest("print_3d_tensor_numpy", test_print_3d_tensor_numpy), &
              new_unittest("print_3d_tensor_mathematica", test_print_3d_tensor_mathematica) &
              ]

   end subroutine collect_pic_matrix_printer_v2_tests

   subroutine test_print_vector_plain(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call pic_print_array(v_sp)
      call pic_print_array(v_dp)
      call pic_print_array(v_int32)
      call pic_print_array(v_int64)
      call pic_print_array(v_sp, "PLAIN")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_plain

   subroutine test_print_vector_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call pic_print_array(v_sp, "NUMPY")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_numpy

   subroutine test_print_vector_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call pic_print_array(v_sp, "MATHEMATICA")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_mathematica

   subroutine test_print_packed_matrix_plain(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: six = 6
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call pic_print_array(v_sp)
      call pic_print_array(v_dp)
      call pic_print_array(v_int32)
      call pic_print_array(v_int64)
      call pic_print_array(v_sp, six, "PLAIN")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, six, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, six, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, six, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_plain

   subroutine test_print_packed_matrix_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: six = 6
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call pic_print_array(v_sp, six, "NUMPY")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, six, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, six, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, six, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_numpy

   subroutine test_print_packed_matrix_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(default_int), parameter :: six = 6
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call pic_print_array(v_sp, six, "MATHEMATICA")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call pic_print_array(v_dp, six, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int32, six, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(v_int64, six, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_mathematica

   subroutine test_print_matrix_plain(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call pic_print_array(m_int32)
      call pic_print_array(m_int64)
      call pic_print_array(m_sp)
      call pic_print_array(m_dp)
      call pic_print_array(m_int32, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_int64, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return
      call pic_print_array(m_sp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_matrix_plain

   subroutine test_print_matrix_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call pic_print_array(m_int32, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_int64, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return
      call pic_print_array(m_sp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_matrix_numpy

   subroutine test_print_matrix_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call pic_print_array(m_int32, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_int64, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
      call pic_print_array(m_sp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(m_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_matrix_mathematica

   subroutine test_print_3d_tensor_plain(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: tensor_int32(2, 2, 2) = &
                                   reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, 7_int32, 8_int32], [2, 2, 2])
      integer(int64), parameter :: tensor_int64(2, 2, 2) = &
                                   reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64, 7_int64, 8_int64], [2, 2, 2])
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])

      call pic_print_array(tensor_int32)
      call pic_print_array(tensor_int64)
      call pic_print_array(tensor_sp)
      call pic_print_array(tensor_dp)

      call pic_print_array(tensor_int32, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_int64, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_sp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_plain

   subroutine test_print_3d_tensor_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: tensor_int32(2, 2, 2) = &
                                   reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, 7_int32, 8_int32], [2, 2, 2])
      integer(int64), parameter :: tensor_int64(2, 2, 2) = &
                                   reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64, 7_int64, 8_int64], [2, 2, 2])
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])

      call pic_print_array(tensor_int32, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_int64, "NUMPY")
      call check(error, .true.)

      if (allocated(error)) return
      call pic_print_array(tensor_sp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_numpy

   subroutine test_print_3d_tensor_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: tensor_int32(2, 2, 2) = &
                                   reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, 7_int32, 8_int32], [2, 2, 2])
      integer(int64), parameter :: tensor_int64(2, 2, 2) = &
                                   reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64, 7_int64, 8_int64], [2, 2, 2])
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])
      call pic_print_array(tensor_int32, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call pic_print_array(tensor_int64, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
      call pic_print_array(tensor_sp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
      call pic_print_array(tensor_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_mathematica

end module test_pic_matrix_printer_v2


program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   use test_pic_matrix_printer_v2, only: collect_pic_matrix_printer_v2_tests
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("pic_matrix_printer_v2", collect_pic_matrix_printer_v2_tests) &
      ]

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if
end program tester

