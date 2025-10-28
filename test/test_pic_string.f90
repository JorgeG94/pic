module test_pic_string
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_io, only: to_char, set_precision, get_precision, pad, &
                     to_upper
   use pic_types, only: int32, int64, dp, sp, default_int
   implicit none
   private

   public :: collect_pic_string_tests

contains

   subroutine collect_pic_string_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      testsuite2 = [ &
                   new_unittest("test_to_char_int32", test_to_char_int32), &
                   new_unittest("test_to_char_int64", test_to_char_int64), &
                   new_unittest("test_to_char_dp", test_to_char_dp), &
                   new_unittest("test_to_char_char", test_to_char_char), &
                   new_unittest("test_to_char_logical", test_to_char_logical), &
                   new_unittest("test_to_char_vector_int32", test_to_char_vector_int32), &
                   new_unittest("test_to_char_vector_int64", test_to_char_vector_int64), &
                   new_unittest("test_to_char_vector_sp", test_to_char_vector_sp), &
                   new_unittest("test_to_char_vector_dp", test_to_char_vector_dp), &
                   new_unittest("test_to_char_matrix_int32", test_to_char_matrix_int32), &
                   new_unittest("test_to_char_matrix_int64", test_to_char_matrix_int64), &
                   new_unittest("test_to_char_matrix_sp", test_to_char_matrix_sp), &
                   new_unittest("test_to_char_matrix_dp", test_to_char_matrix_dp), &
                   new_unittest("test_to_char_sp", test_to_char_sp), &
                   new_unittest("test_set_get_precision", test_set_get_precision), &
                   new_unittest("test_write_with_precision_sp", test_write_with_precision_sp), &
                   new_unittest("test_write_with_precision_dp", test_write_with_precision_dp), &
                   new_unittest("test_padding", test_padding), &
                   new_unittest("test_to_upper", test_to_upper) &
                   ]

   end subroutine collect_pic_string_tests

   subroutine test_to_char_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_char(int(123, kind=int32))
      call check(error, result == "123")
      if (allocated(error)) return
   end subroutine test_to_char_int32

   subroutine test_to_char_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_char(int(12345678, kind=int64))
      call check(error, result == "12345678")
      if (allocated(error)) return
   end subroutine test_to_char_int64

   subroutine test_to_char_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_char(123.456_dp)
      call check(error, result == "123.456000000000")
      if (allocated(error)) return
   end subroutine test_to_char_dp

   subroutine test_to_char_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_char(123.456_sp)
      call check(error, result == "123.456001281738")
      if (allocated(error)) return
   end subroutine test_to_char_sp

   subroutine test_to_char_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result

      result = to_char("Hello, world!")
      call check(error, result == "Hello, world!")
      if (allocated(error)) return
   end subroutine test_to_char_char

   subroutine test_to_char_logical(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5) :: result

      result = to_char(.true.)
      call check(error, result == "TRUE")
      if (allocated(error)) return

      result = to_char(.false.)
      call check(error, result == "FALSE")
      if (allocated(error)) return
   end subroutine test_to_char_logical

   subroutine test_to_char_vector_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      integer(kind=int32), dimension(3) :: vec

      vec = [int(1, kind=int32), int(2, kind=int32), int(3, kind=int32)]
      result = to_char(vec)
   end subroutine test_to_char_vector_int32

   subroutine test_to_char_vector_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      integer(kind=int64), dimension(3) :: vec

      vec = [int(1, kind=int64), int(2, kind=int64), int(3, kind=int64)]
      result = to_char(vec)
   end subroutine test_to_char_vector_int64

   subroutine test_to_char_vector_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      real(kind=sp), dimension(3) :: vec

      vec = [1.0_sp, 2.0_sp, 3.0_sp]
      result = to_char(vec)
   end subroutine test_to_char_vector_sp

   subroutine test_to_char_vector_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      real(kind=dp), dimension(3) :: vec

      vec = [1.0_dp, 2.0_dp, 3.0_dp]
      result = to_char(vec)
   end subroutine test_to_char_vector_dp

   subroutine test_to_char_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      integer(kind=int32), dimension(2, 2) :: mat

      mat = reshape([int(1, kind=int32), int(2, kind=int32), &
                     int(3, kind=int32), int(4, kind=int32)], [2, 2])
      result = to_char(mat)
   end subroutine test_to_char_matrix_int32

   subroutine test_to_char_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      integer(kind=int64), dimension(2, 2) :: mat

      mat = reshape([int(1, kind=int64), int(2, kind=int64), &
                     int(3, kind=int64), int(4, kind=int64)], [2, 2])
      result = to_char(mat)
   end subroutine test_to_char_matrix_int64

   subroutine test_to_char_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      real(kind=sp), dimension(2, 2) :: mat

      mat = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], [2, 2])
      result = to_char(mat)
   end subroutine test_to_char_matrix_sp

   subroutine test_to_char_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result
      real(kind=dp), dimension(2, 2) :: mat

      mat = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
      result = to_char(mat)
   end subroutine test_to_char_matrix_dp

   subroutine test_set_get_precision(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int) :: old_precision, new_precision

      old_precision = get_precision()
      call check(error, old_precision > 0, "Initial precision should be positive")
      new_precision = default_int
      call set_precision(new_precision)
      call check(error, get_precision() == new_precision)
      if (allocated(error)) return

      call set_precision(old_precision)  ! Restore original precision
   end subroutine test_set_get_precision

   subroutine test_write_with_precision_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result
      real(kind=sp), parameter :: value = 123.456_sp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: six = 6_default_int

      call set_precision(six)  ! Set precision for testing
      result = to_char(value)
      expected_len = len_trim(result)
      ! 6 decimal places + 1 for the decimal point + 3 for the number to the right of the decimal point
      call check(error, expected_len == 10, "Expected length of string representation is 10")

      if (allocated(error)) return
   end subroutine test_write_with_precision_sp

   subroutine test_write_with_precision_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result
      real(kind=dp), parameter :: value = 123.456_dp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: six = 6_default_int

      call set_precision(six)  ! Set precision for testing
      result = to_char(value)
      expected_len = len_trim(result)
      print *, "Result: ", expected_len, result
      ! 6 decimal places + 1 for the decimal point + 3 for the number to the right of the decimal point
      call check(error, expected_len == 10, "Expected length of string representation is 10")

      if (allocated(error)) return
   end subroutine test_write_with_precision_dp

   subroutine test_padding(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: result
      real(kind=dp), parameter :: value = 123.456_dp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: three = 3_default_int
      integer(default_int), parameter :: fifteen = 15_default_int

      call set_precision(three)
      result = to_char(value)
      result = pad(result, fifteen)
      expected_len = len_trim(result)
      print *, "Result: ", to_char(expected_len)//to_char(result)
      call check(error, expected_len == fifteen, "expected length of string is 15")
      if (allocated(error)) return
   end subroutine test_padding

   subroutine test_to_upper(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result

      result = to_upper("Hello, world!")
      call check(error, result == "HELLO, WORLD!")
      if (allocated(error)) return

   end subroutine test_to_upper

end module test_pic_string
