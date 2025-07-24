
module test_pic_matrix_printer_v2
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_matrix_printer_v2, only: print_array_v2
   use pic_types, only: sp, dp, int32, int64
   implicit none
   private
   public :: collect_pic_matrix_printer_v2_tests

contains

   subroutine collect_pic_matrix_printer_v2_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      allocate (tests(12))

      tests(1) = new_unittest("print_vector_plain", test_print_vector_plain)
      tests(2) = new_unittest("print_vector_numpy", test_print_vector_numpy)
      tests(3) = new_unittest("print_vector_mathematica", test_print_vector_mathematica)
      tests(4) = new_unittest("print_packed_matrix_plain", test_print_packed_matrix_plain)
      tests(5) = new_unittest("print_packed_matrix_numpy", test_print_packed_matrix_numpy)
      tests(6) = new_unittest("print_packed_matrix_mathematica", test_print_packed_matrix_mathematica)
      tests(7) = new_unittest("print_matrix_plain", test_print_matrix_plain)
      tests(8) = new_unittest("print_matrix_numpy", test_print_matrix_numpy)
      tests(9) = new_unittest("print_matrix_mathematica", test_print_matrix_mathematica)
      tests(10) = new_unittest("print_3d_tensor_plain", test_print_3d_tensor_plain)
      tests(11) = new_unittest("print_3d_tensor_numpy", test_print_3d_tensor_numpy)
      tests(12) = new_unittest("print_3d_tensor_mathematica", test_print_3d_tensor_mathematica)

   end subroutine collect_pic_matrix_printer_v2_tests

   subroutine test_print_vector_plain(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call print_array_v2(v_sp, "PLAIN")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_plain

   subroutine test_print_vector_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call print_array_v2(v_sp, "NUMPY")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_numpy

   subroutine test_print_vector_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(3) = [1.0_sp, 2.0_sp, 3.0_sp]
      real(dp), parameter :: v_dp(3) = [1.0_dp, 2.0_dp, 3.0_dp]
      integer(int32), parameter :: v_int32(3) = [1_int32, 2_int32, 3_int32]
      integer(int64), parameter :: v_int64(3) = [1_int64, 2_int64, 3_int64]

      call print_array_v2(v_sp, "MATHEMATICA")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_vector_mathematica

   subroutine test_print_packed_matrix_plain(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call print_array_v2(v_sp, 6, "PLAIN")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, 6, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, 6, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, 6, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_plain

   subroutine test_print_packed_matrix_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call print_array_v2(v_sp, 6, "NUMPY")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, 6, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, 6, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, 6, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_numpy

   subroutine test_print_packed_matrix_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: v_sp(6) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp]
      real(dp), parameter :: v_dp(6) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
      integer(int32), parameter :: v_int32(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]
      integer(int64), parameter :: v_int64(6) = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]

      call print_array_v2(v_sp, 6, "MATHEMATICA")
      call check(error, .true.)  ! Assuming the print is correct, no error expected
      if (allocated(error)) return

      call print_array_v2(v_dp, 6, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int32, 6, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(v_int64, 6, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_packed_matrix_mathematica

   subroutine test_print_matrix_plain(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call print_array_v2(m_int32, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_int64, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return
      call print_array_v2(m_sp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_matrix_plain

   subroutine test_print_matrix_numpy(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call print_array_v2(m_int32, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_int64, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return
      call print_array_v2(m_sp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_matrix_numpy

   subroutine test_print_matrix_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: m_int32(2, 2) = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
      integer(int64), parameter :: m_int64(2, 2) = reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2])
      real(sp), parameter :: m_sp(2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      real(dp), parameter :: m_dp(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])

      call print_array_v2(m_int32, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_int64, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
      call print_array_v2(m_sp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(m_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return
   end subroutine test_print_matrix_mathematica

   subroutine test_print_3d_tensor_plain(error)
      type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])

      call print_array_v2(tensor_sp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(tensor_dp, "PLAIN")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_plain

   subroutine test_print_3d_tensor_numpy(error)
      type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])

      call print_array_v2(tensor_sp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(tensor_dp, "NUMPY")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_numpy

   subroutine test_print_3d_tensor_mathematica(error)
      type(error_type), allocatable, intent(out) :: error
    real(sp), parameter :: tensor_sp(2, 2, 2) = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp], [2, 2, 2])
    real(dp), parameter :: tensor_dp(2, 2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2, 2, 2])

      call print_array_v2(tensor_sp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

      call print_array_v2(tensor_dp, "MATHEMATICA")
      call check(error, .true.)
      if (allocated(error)) return

   end subroutine test_print_3d_tensor_mathematica

end module test_pic_matrix_printer_v2
